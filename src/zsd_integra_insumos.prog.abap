*&---------------------------------------------------------------------*
*& Report  ZSD_INTEGRA_AGRIQ
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT zsd_integra_insumos.

*******************************************************************************************
* TELA SELECAO
*******************************************************************************************
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
PARAMETERS: p_nrvend TYPE znr_venda MODIF ID t1 OBLIGATORY,
            p_tpdoc  TYPE ztipo_doc MODIF ID t1 OBLIGATORY.
SELECTION-SCREEN END OF BLOCK b1.

TRY .
*    zcl_integracao_insumos=>zif_integracao_insumos~get_instance(
*       )->set_criar_documento( EXPORTING i_nr_venda  = p_nrvend
*                                         i_tipo_doc  = p_tpdoc ).

  CATCH zcx_integracao INTO DATA(ex_integra).
    ex_integra->zif_error~published_erro( i_msgty = 'S' ). " i_msgty_display = 'E' ).

  CATCH zcx_error INTO DATA(ex_error).    "  "
    ex_error->zif_error~published_erro(   i_msgty = 'S' ). "i_msgty_display = 'E' ).

ENDTRY.
