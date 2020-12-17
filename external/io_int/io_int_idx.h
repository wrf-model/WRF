/*
 *
 * Public domain.
 *
 */

/**
 * \file io_int_idx.h
 * Definitions and data structures for WRF IO Internal.
 **/

#ifndef WRF_IO_INT_IDX_H
#define WRF_IO_INT_IDX_H

#ifdef __cplusplus
extern "C" {
#endif

#ifndef MAP_FILE		/* 4.3+BSD defines & needs for  */
#  define MAP_FILE 0		/* mmap on files, all others don't */
#endif

/** Maximum length of a string.
 * This has been equivalently defined in Fortran.
 **/
#define STR_LEN 132

/** Record meta information. **/
struct r_info {
	int64_t offset;      /**< Offset to position in the file **/
	int64_t data_off;    /**< Offset to data **/
	int32_t data_count;  /**< Number of data elements **/
	int32_t data_type;   /**< WRF type of data elements **/
	char name[STR_LEN];  /**< Name of the variable **/
	char date[STR_LEN];  /**< Date of the variable **/
};

/** Create an inventory of a WRF IO Internal file **/
int32_t io_int_index(const char *, struct r_info **, int32_t *);

/** Lookup the data offset and count for a record **/
int32_t io_int_loc(const char *, struct r_info *, int32_t, int64_t *, int32_t *);

#ifdef __cplusplus
}				/* extern "C" */
#endif
#endif				/* WRF_IO_INT_IDX_H */
