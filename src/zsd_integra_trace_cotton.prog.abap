*&---------------------------------------------------------------------*
*& Report  ZSD_INTEGRA_AGRIQ
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT zsd_integra_trace_cotton.

TABLES: zsdt0045, zsdt0143.

*******************************************************************************************
* TELA SELECAO
*******************************************************************************************
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
PARAMETERS: p_zseq   TYPE zsdt0045-zseq_inst, " OBLIGATORY,
            p_objek  TYPE zsdt0045-objek    , " OBLIGATORY,
            p_obtab  TYPE zsdt0045-objecttable , "OBLIGATORY.
            p_contr  TYPE zsdt0143-id_contrato, " OBLIGATORY.
            p_docnum TYPE zsdt0330-docnum. " OBLIGATORY.
SELECTION-SCREEN END OF BLOCK b1.

TRY .
    zcl_trace_cotton=>zif_trace_cotton~get_instance(
       )->set_envia_nf_cancelada( EXPORTING i_docnum = p_docnum ).

*    IF 1 = 1.
*      zcl_trace_cotton=>zif_trace_cotton~get_instance(
*         )->set_cadastra_instrucao( EXPORTING i_zseq_inst   = p_zseq
*                                              i_objek       = p_objek
*                                              i_objecttable = p_obtab ).
*    ELSE.
*      zcl_trace_cotton=>zif_trace_cotton~get_instance(
*         )->set_exclui_instrucao( EXPORTING i_zseq_inst   = p_zseq
*                                            i_objek       = p_objek
*                                            i_objecttable = p_obtab ).
*    ENDIF.

*    IF 1 = 1.
*      zcl_trace_cotton=>zif_trace_cotton~get_instance(
*         )->set_cadastra_contratos( EXPORTING i_id_contrato = p_contr ).
*    ELSE.
*      zcl_trace_cotton=>zif_trace_cotton~get_instance(
*         )->set_exclui_contratos( EXPORTING i_id_contrato = p_contr ).
*    ENDIF.

  CATCH zcx_integracao INTO DATA(ex_integra).
    ex_integra->zif_error~published_erro( i_msgty = 'S' ). " i_msgty_display = 'E' ).

  CATCH zcx_error INTO DATA(ex_error).    "  "
    ex_error->zif_error~published_erro(   i_msgty = 'S' ). "i_msgty_display = 'E' ).

ENDTRY.
