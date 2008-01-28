#!/bin/ksh
# Create community release of WRFVAR by removing elements we don't
# want to release, and remove subversion information that would allow
# them to be recreated.
#
# Remove CRTM and RTTOV
#
find . -name .svn -exec rm -rf {} \; >/dev/null 2>&1
rm -rf da/da_radiance/*

cat > da/da_radiance/da_radiance.f90 <<EOF
! Stub, as CRTM and RTTOV radiance models not included in community release
module da_radiance
end module da_radiance
EOF

cat > da/da_radiance/da_radiance1.f90 <<EOF
! Stub, as CRTM and RTTOV radiance models not included in community release
module da_radiance1
end module da_radiance1
EOF

cat > da/da_radiance/da_crtm.f90 <<EOF
! Stub, as CRTM radiance model not included in community release
module da_crtm
end module da_crtm
EOF

cat > da/da_radiance/da_rttov.f90 <<EOF
! Stub, as RTTOV radiance model not included in community release
module da_rttov
end module da_rttov
EOF

cat > da/da_radiance/gamma1.f90 <<EOF
! Stub, as radiance model not included in community release
EOF

cat > da/da_radiance/gsi_kinds.f90 <<EOF
! Stub, as radiance model not included in community release
EOF

cat > da/da_radiance/gsi_constants.f90 <<EOF
! Stub, as radiance model not included in community release
EOF

cat > da/da_radiance/gsi_thinning.f90 <<EOF
! Stub, as radiance model not included in community release
EOF

cat > da/da_radiance/module_radiance.f90 <<EOF
! Stub, as radiance model not included in community release
EOF

