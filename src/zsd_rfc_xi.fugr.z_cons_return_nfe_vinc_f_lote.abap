FUNCTION Z_CONS_RETURN_NFE_VINC_F_LOTE.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(I_DOCNUM) TYPE  J_1BDOCNUM
*"  EXPORTING
*"     VALUE(R_VINCFLOTE) TYPE  ZSDTVINC_P_FLOTE_T
*"----------------------------------------------------------------------

  DATA: r_docnum  TYPE RANGE OF j_1bdocnum.

  APPEND VALUE #(
               sign    = zcl_les_utils=>if_stab_constants~mc_sign_include
               option  = zcl_les_utils=>if_stab_constants~mc_option_equal
               low     = I_DOCNUM
             ) TO r_docnum.

CALL METHOD ZCL_IM_CL_FLUXO_EXPORTACAO=>RETURN_NFE_VINC_F_LOTE
  EXPORTING
    I_DOCNUM    = r_docnum
*    I_DATA     =
  RECEIVING
    R_VINCFLOTE = R_VINCFLOTE
    .


DELETE R_VINCFLOTE WHERE CANCEL IS NOT INITIAL.


ENDFUNCTION.
