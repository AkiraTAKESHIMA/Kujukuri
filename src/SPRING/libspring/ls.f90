module libspring
  use ls_base, only: &
        spring_set_logopt
  use ls_gs, only: &
        spring_define_mesh_latlon, &
        spring_define_mesh_raster, &
        spring_clear_mesh        , &
        spring_print_meshes_name , &
        spring_print_mesh
  use ls_rt, only: &
        spring_make_rmptbl      , &
        spring_clear_rmptbl     , &
        spring_get_rmptbl_length, &
        spring_get_rmptbl_data  , &
        spring_print_rmptbl_name, &
        spring_print_rmptbl
  use ls_remap, only: &
        spring_remap
  use ls_manage, only: &
        spring_initialize, &
        spring_finalize
  use ls_log, only: &
        spring_print_error_message
  implicit none
end module libspring
