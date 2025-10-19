function z_pfe_ajuste_zlest0018.
*"----------------------------------------------------------------------
*"*"Interface local:
*"----------------------------------------------------------------------


  update zlest0018
     set tipcontabil = 'FC'
   where tipcontabil eq space.

  commit work.

endfunction.
