FUNCTION zsd_insumos_distribuicao.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(I_ZSDS093) TYPE  ZSDS093
*"  TABLES
*"      T_ZSDS094 TYPE  ZSDS094_TT
*"----------------------------------------------------------------------

  CREATE OBJECT lc_distribuicao_insumos.

  lc_zsds093   = i_zsds093.
  lc_zsds094[] = t_zsds094[].

*---------------------------------
* efetua distribuicao
*---------------------------------
  TRY.
      lc_distribuicao_insumos->get_instance(
        )->set_status_processamento(  EXPORTING i_nro_sol = lc_zsds093-nro_sol i_seq     = lc_zsds093-seq i_vbeln       = lc_zsds093-vbeln
                                                i_posnr   = lc_zsds093-posnr   i_status  = abap_true
        )->set_executar_distribuicao( EXPORTING i_zsds093 = lc_zsds093         i_zsds094 = lc_zsds094     i_reprocessar = abap_true
        )->set_status_processamento(  EXPORTING i_nro_sol = lc_zsds093-nro_sol i_seq     = lc_zsds093-seq i_vbeln       = lc_zsds093-vbeln
                                                i_posnr   = lc_zsds093-posnr   i_status  = abap_false
        ).
    CATCH zcx_error INTO DATA(ex_error).
  ENDTRY.

  lc_distribuicao_insumos->set_status_processamento(  EXPORTING i_nro_sol = lc_zsds093-nro_sol i_seq   = lc_zsds093-seq
                                                                i_vbeln   = lc_zsds093-vbeln   i_posnr = lc_zsds093-posnr
                                                                i_status  = abap_false ).

ENDFUNCTION.
