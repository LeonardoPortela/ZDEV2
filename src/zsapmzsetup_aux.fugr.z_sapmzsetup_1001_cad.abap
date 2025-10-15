function z_sapmzsetup_1001_cad.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(P_CD_PARAM) TYPE  POSNR OPTIONAL
*"     REFERENCE(P_EXCLUIR) TYPE  CHAR1 DEFAULT ' '
*"----------------------------------------------------------------------

  tables zmmt_ee_zgr_imp.

  clear: zmmt_ee_zgr_imp.

  if ( not p_cd_param is initial ).

    if not p_excluir is initial.
      delete from zmmt_ee_zgr_imp where cd_param eq p_cd_param.
      exit.
    else.
      select single * into zmmt_ee_zgr_imp
        from zmmt_ee_zgr_imp
       where cd_param eq p_cd_param.
    endif.

  endif.

  call screen 0001 starting at 05 05 ending at 70 12.

endfunction.
