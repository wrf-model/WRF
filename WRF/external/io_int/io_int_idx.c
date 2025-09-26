/*
 *
 * Public domain.
 *
 */

/* System Includes */
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/mman.h>

/* Standard Library Includes */
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <fcntl.h>
#include <unistd.h>
#include <arpa/inet.h>

#include "io_int_idx.h"
#include "io_int_idx_tags.h"

/* Static/Private functions within this file */

/** Mmap the file **/
static int32_t io_int_fmmap(const char *, int32_t **, int64_t *);

/** Un-mmap the file **/
static void io_int_funmmap(int32_t *, int64_t, int32_t);

/** Get the number of records in a file **/
static int32_t io_int_nrecords(int32_t *, int64_t, int32_t *);

/** Get the record name form record data **/
static int32_t io_int_record_name(int32_t *, int32_t, int32_t,
				      char *[STR_LEN]);

/** Get the record date form record data **/
static int32_t io_int_record_date(int32_t *, int32_t, int32_t,
				      char *[STR_LEN]);

/** Get the data offset/position for a record */
static int32_t io_int_record_data_pos(int32_t *, int32_t, int32_t,
					  int64_t *, int32_t *);

/** Trim white space off the end of a string */
static int32_t trim(const char *s1, char *s2);

/* Public functions within this file */

/**
 * Create an index of a WRF binary IO file. This populates an array
 * of records. Where each record contains
 * - \e offset The absolute file offset to the record header
 * - \e data_off The absolute file offset to the data for the record
 * - \e data_count The number of data items (i.e. array length)
 * - \e name The records name
 * - \e date The date of the record
 *
 * \param[in] filename The file to index.
 * \param[out] records An aray of records from the file.
 * \param[out] nrecords The number of records.
 *
 * \retval 0 If it was sucessful.
 * \retval 1 If there was an error.
 **/
int32_t
io_int_index(const char *filename, struct r_info **records,
		 int32_t *nrecords)
{
	int32_t ifd = 0;	/* File descriptor           */
	int32_t *fmap = NULL;	/* Mmap file base address    */
	int32_t n = 0;		/* Number of records decoded */
	int32_t rlen = 0;	/* Record length             */
	int32_t type = 0;	/* Record data type          */
	int64_t len = 0;	/* Lenght of the file        */
	off_t pos = 0;		/* Current position in array */
	char *ptr = NULL;	/* Temporary point to string */

	/* If we don't have a filename get out early */
	if (filename == NULL) {
		fprintf(stderr, "unable to index null filename");
		return (EXIT_FAILURE);
	}

#if 0   /* XLF & XLC don't play nice! If you create a pointer
         * in fortran, nullify it and pass it in here, xlc says
	 * it is not NULL. While GNU, Intel and PGI do....
         */
	/* If we can't write to the record list get out */
	if (*records != NULL) {
		fprintf(stderr,
			"will not be able to write to the record pointer");
		return (EXIT_FAILURE);
	}
#endif

	/* Get the file mmap'ng */
	if ((ifd = io_int_fmmap(filename, &fmap, &len)) < 0) {
		return (EXIT_FAILURE);
	}

	/* Get the number of records in the file */
	if (io_int_nrecords(fmap, len, nrecords)) {
		return (EXIT_FAILURE);
	}

	/* Get the memory for the records */
	if ((*records = malloc(sizeof(struct r_info) * *nrecords)) == NULL) {
		fprintf(stderr, "unable to malloc record information");
		return (EXIT_FAILURE);
	}

	/* Zero it all out */
	memset(*records, 0, sizeof(struct r_info) * *nrecords);

	while (pos < len) {
		/* Get the record len @ start */
		rlen = ntohl(fmap[pos]) / sizeof(int32_t);
		if (rlen == 512) {	/* Only look at headers */
			type = ntohl(fmap[pos + 2]);
			switch (type) {
			case INT_FIELD:
			case INT_DOM_TI_INTEGER:
			case INT_DOM_TI_CHAR:
			case INT_DOM_TI_REAL:
				(*records)[n].offset =
				    (pos + 1) * sizeof(int32_t);
				ptr = (*records)[n].name;
				io_int_record_name(fmap, pos, rlen, &ptr);
				ptr = (*records)[n].date;
				io_int_record_date(fmap, pos, rlen, &ptr);
				io_int_record_data_pos(fmap, pos, rlen,
							   &((*records)[n].
							     data_off),
							   &((*records)[n].
							     data_count));
				(*records)[n].data_type = type;
				++n;
				break;
			default:
				break;
			}
		}
		/* Move ourselves to the next record */
		pos += rlen + 2;
	}

	/* Reset the number of records if we only have offsets for a subset */
	*nrecords = n;

	/* Close and unmap the file */
	io_int_funmmap(fmap, len, ifd);

	return (EXIT_SUCCESS);
}

/**
 * Lookup the data offset and count for a record. This method
 * should be called after an index has been created for the file.
 *
 * \param[in] var     The record variable name.
 * \param[in] records The array containing all the record structures.
 * \param[in] n       The number of records in the array.
 * \param[out] offset The absolute file offset to the start of the data.
 * \param[out] count  The number of data elements.
 *
 * \retval 0 If it was sucessful.
 * \retval 1 If there was an error.
 **/
int32_t
io_int_loc(const char *var, struct r_info * records, int32_t n,
	       int64_t *offset, int32_t *count)
{
	int32_t i = 0;
	char tmp[STR_LEN] = {0};

	for (i = 0; i <= n; ++i) {
		trim(records[i].name, tmp);
		if (strcmp(var, tmp) == 0) {
			*offset = records[i].data_off;
			*count = records[i].data_count;
			return (EXIT_SUCCESS);
		}
	}
	return (EXIT_FAILURE);
}

/**
 * Memory map a WRF binary IO file. This will place the file
 * starting memory address in \e fmap .
 *
 * \param[in] filename The name of the WRF binary IO file.
 * \param[out] fmap    The base address of the mapped file.
 * \param[out] len     The length of the mapped file.
 *
 * \returns ifd        The file descriptor associated with the mmapping.
 * \retval -1          If there was an error.
 **/
static int32_t
io_int_fmmap(const char *filename, int32_t ** fmap, int64_t * len)
{
	int32_t ifd = 0;		/* File descriptor       */
	const int32_t error = -1;	/* Returned error number */
	struct stat ibuf = { 0 };	/* File stat information */

	/* If we have mapped a file before */
	if (*fmap) {
		return (error);
	}

	/* Open the file for reading */
	if ((ifd = open(filename, O_RDONLY)) < 0) {
		fprintf(stderr, "unable to open file: %s", filename);
		return (error);
	}

	/* Get the size of the file */
	if (fstat(ifd, &ibuf) == -1) {
		fprintf(stderr, "unable to stat: %s", filename);
		goto rtn_err;
	}

	/* If the file is zero size, leave */
	if (ibuf.st_size == 0) {
		fprintf(stderr, "will not index 0 byte file: %s", filename);
		goto rtn_err;
	}

	/* Map the file into memory */
	if ((*fmap = (int32_t *) mmap(0, ibuf.st_size, PROT_READ,
				      MAP_FILE | MAP_SHARED, ifd,
				      0)) == MAP_FAILED) {
		fprintf(stderr, "unable to memory map file: %s", filename);
		goto rtn_err;
	}

	/* Advise the kernel that we will be randomly accessing the file.
	 * We don't check for an error as AIX doesn't honour madvise anyway. */
	madvise((caddr_t) * fmap, ibuf.st_size, MADV_RANDOM);

	/* Set the file size in int32_t units */
	*len = ibuf.st_size / sizeof(int32_t);

	/* Return the file descriptor */
	return (ifd);

 rtn_err:
	if (close(ifd)) {
		fprintf(stderr, "unable to close file %s", filename);
	}
	return (error);
}

/**
 * Unmap the file from memory.
 *
 * \param[in] fmap    The base address of the mapped file.
 * \param[in] len     The length of the mapped file.
 * \param[in] ifd     The file descriptor.
 *
 **/
static void
io_int_funmmap(int32_t * fmap, int64_t len, int32_t ifd)
{

	if (fmap == NULL) {
		return;
	}

	/* Release the memory */
	if (munmap(fmap, len * sizeof(int32_t)) == -1) {
		fprintf(stderr, "unable to un-mmap() file");
	}
	fmap = NULL;

	/* Close the file */
	if (close(ifd)) {
		fprintf(stderr, "unable to close the mmap()-ed file");
	}

	return;
}

/**
 * Count the number of records in a WRF binary IO file.
 *
 * \param[in] fmap    The base address of the mapped file.
 * \param[in] len     The length of the mapped file.
 * \param[out] nrecords The number of records in the file.
 *
 * \retval 0 If it was sucessful.
 * \retval 1 If there was an error in couting the records.
 **/
static int32_t
io_int_nrecords(int32_t * fmap, int64_t len, int32_t * nrecords)
{

	int32_t rfs = 0;	/* Record size at front      */
	int32_t res = 0;	/* Record size at end        */
	off_t pos = 0;		/* Current position in array */
	off_t end = 0;		/* End of current record     */

	/* Set the number of seen records to 0 */
	*nrecords = 0;

	/* Make sure we have a mapping */
	if (fmap == NULL || len == 0) {
		return (EXIT_FAILURE);
	}

	/* Start counting */
	while (pos < len) {
		/* Incr. the record number */
		++(*nrecords);
		/* Get the record len @ start */
		rfs = ntohl(fmap[pos]) / sizeof(int32_t);
		/* Get the end position */
		end = pos + rfs + 1;

		/* Make sure we are not going outside our area */
		if (end >= len) {
			fprintf(stderr, "record %d end is beyond mapped file",
				*nrecords);
			return (EXIT_FAILURE);
		}
		/* Get the record len @ end */
		res = ntohl(fmap[end]) / sizeof(int32_t);

		/* Make sure the start and end record lengths match */
		if (rfs != res) {
			fprintf(stderr,
			   "record %d start and end size mismatch (%d != %d)",
				*nrecords, rfs, res);
			return (EXIT_FAILURE);
		}

		/* Make sure the record size is not zero */
		if (rfs == 0) {
			fprintf(stderr, "record %d has a size of 0 at %ld",
				*nrecords, pos);
			return (EXIT_FAILURE);
		}

		/* Move ourselves to the next record */
		pos = end + 1;
	}
	return (EXIT_SUCCESS);
}

/**
 * Reads a record name from the record.
 *
 * This function is a cheap alternative to all the functions in
 * ext/io_int/module_internal_header_util.F
 *
 * The data field normally consists of:
 * - data[0]             Record size
 * - data[1]             Header size
 * - data[2]             Data record WRF IO type
 * - data[3]             Data type size
 * - data[4]             Data handle
 * - data[5]             Length of the date string (i.e. n)
 * - data[6:6+n]         Date string
 * - data[6+n+1]         Length of the name string (i.e. m)
 * - data[6+n+2:6+n+2+m] Name string
 *
 * \param[in]  fmap The base address of the mapped file.
 * \param[in]  pos  The starting position in the fmap array.
 * \param[in]  size The size of the record.
 * \param[out] name The name of the variable in the record, to a
 *                  maximum length of STR_LEN.
 *
 * \retval 0 If there name was read successfully.
 * \retval 1 If there was an error reading the name.
 **/
static int32_t
io_int_record_name(int32_t * fmap, int32_t pos, int32_t size,
		       char *(name[STR_LEN]))
{

	int32_t doff = 5;	/* Date length offset in data */
	int32_t dlen = 0;	/* Date string length         */
	int32_t nlen = 0;	/* Name string length         */
	int32_t i = 0;		/* Temproary loop indexer     */

	/* Make sure we have a file array */
	if (fmap == NULL) {
		return (EXIT_FAILURE);
	}

	/* Check to make sure the date length is within our data.
	 * Read the date string length. */
	if (size >= doff) {
		dlen = ntohl(fmap[pos + doff]);
	} else {
		return (EXIT_FAILURE);
	}

	/* Check to make sure the name length is within our data.
	 * Read the name string length */
	if (size >= doff + dlen + 1) {
		nlen = ntohl(fmap[pos + doff + dlen + 1]);
	} else {
		return (EXIT_FAILURE);
	}

	/* Check to make sure the whole name string is within our data. */
	if (size <= (doff + dlen + 1 + nlen)) {
		return (EXIT_FAILURE);
	}

	/* If the name string is zero move onto the next record */
	if (nlen <= 0) {
		return (EXIT_FAILURE);
	}

	/* Blank out the name (play nice with Fortran) */
	memset(*name, 32, STR_LEN);

	/* Un-wrap the name */
	for (i = 0; i < nlen; ++i) {
		(*name)[i] = (char)ntohl(fmap[pos + doff + dlen + 2 + i]);
	}

	return (EXIT_SUCCESS);
}

/**
 * Reads a record date from the record.
 *
 * This function is a cheap alternative to all the functions in
 * ext/io_int/module_internal_header_util.F
 *
 * The data field normally consists of:
 * - data[0]             Record size
 * - data[1]             Header size
 * - data[2]             Data record WRF IO type
 * - data[3]             Data type size
 * - data[4]             Data handle
 * - data[5]             Length of the date string (i.e. n)
 * - data[6:6+n]         Date string
 *
 * \param[in]  fmap The base address of the mapped file.
 * \param[in]  pos  The starting position in the fmap array.
 * \param[in]  size The size of the record.
 * \param[out] date The date of the variable in the record, to a
 *                  maximum length of STR_LEN.
 *
 * \retval 0 If there name was read successfully.
 * \retval 1 If there was an error reading the name.
 **/
static int32_t
io_int_record_date(int32_t * fmap, int32_t pos, int32_t size,
		   char *(date[STR_LEN]))
{

	int32_t wtype = 0;	/* WRF header type            */
	const int32_t doff = 5;	/* Date length offset in data */
	int32_t dlen = 0;	/* Date string length         */
	int32_t i = 0;		/* Temproary loop indexer     */

	/* Make sure we have a file array */
	if (fmap == NULL) {
		return (EXIT_FAILURE);
	}

	wtype = ntohl(fmap[pos + 2]);
	if ((wtype == INT_DOM_TI_INTEGER) ||
	    (wtype == INT_DOM_TI_CHAR)    ||
	    (wtype == INT_DOM_TI_REAL)
	    ) {
		/* Blank out the name (play nice with Fortran) */
		memset(*date, 32, STR_LEN);
		return (EXIT_SUCCESS);
	}

	/* Check to make sure the date length is within our data.
	 * Read the date string length. */
	if (size >= doff) {
		dlen = ntohl(fmap[pos + doff]);
	} else {
		return (EXIT_FAILURE);
	}

	if (dlen > 1) {
		/* Blank out the name (play nice with Fortran) */
		memset(*date, 32, STR_LEN);
		/* Un-wrap the name */
		for (i = 0; i < dlen; ++i) {
			(*date)[i] = (char)ntohl(fmap[pos + doff + 1 + i]);
		}
	}

	return (EXIT_SUCCESS);
}

/**
 * Reads a record data offset and count from the record header.
 *
 * This function is a cheap alternative to all the functions in
 * ext/io_int/module_internal_header_util.F
 *
 * The data field normally consists of:
 * - data[0]             Record size
 * - data[1]             Header size
 * - data[2]             Data record WRF IO type
 * - data[3]             Data type size
 * - data[4]             Data handle
 * - data[5]             Length of the date string (i.e. n)
 * - data[6:6+n]         Date string
 *
 * \param[in]  fmap   The base address of the mapped file.
 * \param[in]  pos    The starting position in the fmap array.
 * \param[in]  size   The size of the record.
 * \param[out] offset The offset to the data.
 * \param[out] count  The number of elements in the data.
 *
 * \retval 0 If there name was read successfully.
 * \retval 1 If there was an error reading the name.
 **/
static int32_t
io_int_record_data_pos(int32_t *fmap, int32_t pos, int32_t size,
			int64_t *offset, int32_t *count)
{

	int32_t wtype = 0;	/* WRF field type */
	int32_t dtype = 0;	/* Data type */
	int32_t elen  = 0;	/* Char element length (normally 0) */
	int32_t dlen  = 0;	/* Char data (name) length */
	int32_t vlen  = 0;	/* Char value length */

	/* Zero out the offset and count */
	*offset = 0;
	*count  = 0;

	wtype = ntohl(fmap[pos + 2]);
	switch (wtype) {
	case INT_FIELD:
		dtype = ntohl(fmap[pos + 3]);
		/* The data is the next record */
		*offset = (pos + size + 3) * sizeof(int32_t);
		/* 3 => two int's for header record length  *
		 *      one int for data record length      */
		*count = ntohl(fmap[pos + size + 2]) / dtype;
		break;
	case INT_DOM_TI_REAL:
	case INT_DOM_TI_INTEGER:
		dtype = ntohl(fmap[pos + 4]);
		*offset = (pos + 6) * sizeof(int32_t);
		*count = (ntohl(fmap[pos + 4]) * ntohl(fmap[pos + 5])) / dtype;
		break;
	case INT_DOM_TI_CHAR:
		dtype = ntohl(fmap[pos + 4]);
		elen = ntohl(fmap[pos + 5]);               /* element */
		dlen = ntohl(fmap[pos + elen + 6]);        /* name */
		vlen = ntohl(fmap[pos + elen + dlen + 7]); /* vlaue */
		*offset = (pos + elen + dlen + 8) * sizeof(int32_t);
		*count = vlen / dtype;
		break;
	default:
		fprintf(stderr, "Unsupport record type: %d\n", wtype);
		return (EXIT_FAILURE);
	}
	return (EXIT_SUCCESS);

}

/**
 * Trim a string. The strings in \e io_int_idx have been space
 * padded to play nice with Fortran, they are not null terminated
 * for C. This function will trim off any trailing spaces.
 *
 * A word of warning. No checks are made to make sure s2 can
 * hold the resultant string.
 *
 * \param[in]  s1  The space padded string.
 * \param[out] s2  The non-space padded string.
 *
 * \retval 0 If there were no errors.
 * \retval 1 If there was an error.
 **/
static int32_t
trim(const char *s1, char *s2)
{
	int32_t i = 0;

	if (!s1) {
		return(EXIT_FAILURE);
	}
	for (i = STR_LEN -1; i >= 0; --i) {
		if ( s1[i] != 32) {
			break;
		} else {
			s2[i] = '\0';
		}
	}
	strncpy(s2,s1,i+1);

	return(EXIT_SUCCESS);
}

