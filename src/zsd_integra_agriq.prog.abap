*&---------------------------------------------------------------------*
*& Report  ZSD_INTEGRA_AGRIQ
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT zsd_integra_agriq.

*DELETE FROM zsdt0299.
*COMMIT WORK.
*EXIT.
*BREAK-POINT.

*******************************************************************************************
* TELA SELECAO
*******************************************************************************************
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
PARAMETERS: p_nrocgd TYPE znro_cg MODIF ID t1 OBLIGATORY,
            p_chref  TYPE zch_ref MODIF ID t1 OBLIGATORY.
*           p_docnum TYPE j_1bdocnum MODIF ID t1.
SELECTION-SCREEN END OF BLOCK b1.

TRY .
*    zcl_integracao_agriq=>zif_integracao_agriq~get_instance(
*       )->set_exec_agriq( EXPORTING i_metodo = 'GET_MAPA'
*                                    i_codmapa = '06105x').

*    zcl_integracao_agriq=>zif_integracao_agriq~get_instance(
*       )->set_gerar_sol_ra( EXPORTING i_nro_cgd       = 18026
*                                      i_ch_referencia = 'RO00000000408462').

*    zcl_integracao_agriq=>zif_integracao_agriq~get_instance(
*       )->set_atualiza_nfe_receita( EXPORTING i_docnum = p_docnum ).

    zcl_integracao_agriq=>zif_integracao_agriq~get_instance(
       )->set_consultar_sol_ra( EXPORTING i_nro_cgd       = p_nrocgd
                                          i_ch_referencia = p_chref ).

*    zcl_integracao_agriq=>zif_integracao_agriq~get_instance(
*       )->set_cancelar_sol_ra( EXPORTING i_nro_cgd       = p_nrocgd
*                                         i_ch_referencia = p_chref ).

  CATCH zcx_integracao INTO DATA(ex_integra).
    ex_integra->zif_error~published_erro( i_msgty = 'S' ). " i_msgty_display = 'E' ).

  CATCH zcx_error INTO DATA(ex_error).    "  "
    ex_error->zif_error~published_erro(   i_msgty = 'S' ). "i_msgty_display = 'E' ).

ENDTRY.
