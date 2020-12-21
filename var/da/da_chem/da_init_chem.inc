subroutine da_init_chem(nshx, nahx, ncs, nca, pfs, nsa, nsb)

   integer :: nshx, nahx, ncs, nca, pfs, nsa, nsb

   num_surf_hx = nshx
   num_acft_hx = nahx
   num_chem_surf = ncs
   num_chem_acft = nca
   PARAM_FIRST_SCALAR = pfs
   num_scaleant = nsa
   num_scalebb = nsb

end subroutine da_init_chem

