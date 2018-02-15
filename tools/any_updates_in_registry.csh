#!/bin/csh

unalias rm cp

#	Easy way to get this hash is to do a "git log README" in the top WRFV3 directory

set LAST_RELEASE_HASH = a98c4ada98934ef0

if ( -e newbies ) then
	rm -rf newbies
endif
touch newbies

pushd ../Registry >& /dev/null
set Registry_files = `ls -1 Registry* registry*`
popd >& /dev/null

foreach f ( $Registry_files )
	if ( ( $f == Registry.CONVERT ) || \
	     ( $f == Registry.EM_COMMON.var ) || \
	     ( $f == Registry.NMM ) || \
	     ( $f == registry.chem ) || \
	     ( $f == registry.tracker ) || \
	     ( $f == registry.var ) || \
	     ( $f == registry.var_chem ) || \
	     ( $f == Registry ) ) then
	else

		#	Check existend of file within last release

		git diff ${LAST_RELEASE_HASH}:Registry/$f -- ../Registry/$f >& /dev/null
		set OK = $status
		if ( $OK == 0 ) then

			#	Yep, it existed, process the diffs

			git diff ${LAST_RELEASE_HASH}:Registry/$f -- ../Registry/$f >& ad.${f}.txt

			#	Check if this is an empty file

			ls -ls ad.${f}.txt | grep " 0 " >& /dev/null
			set ZERO_SIZED = $status
			if ( $ZERO_SIZED == 0 ) then

				#	Empty file, remove it from consideration

				rm -rf ad.${f}.txt
			else

				#	Check for new rconfig entries

				grep "^+" ad.${f}.txt | grep -i rconfig >& /dev/null
				set NEW_STUFF = $status
				if ( $NEW_STUFF == 0 ) then

					#	Yep, found some rconfigs with a leading plus, international sign of adding something or other

					echo " "
					echo processing $f

					#	Check to see if this new rconfig is talked about in the README.namelist

					foreach g ( `grep "^+" ad.${f}.txt | grep -i rconfig | awk '{print $3}' ` )
					
						grep -iq $g ../run/README.namelist
						set ALREADY_DESCRIBED = $status

						if ( $ALREADY_DESCRIBED != 0 ) then
							echo $g is not described in README.namelist
						endif
					end

					
				endif
				
			endif

		else
		
			#	Nope, did not previously exist.  New file needs to handled separately

			echo $f >> newbies
		endif
	endif
end
