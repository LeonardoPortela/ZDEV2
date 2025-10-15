*&---------------------------------------------------------------------*
*& Report  ZSD_INTEGRA_AGRIQ
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT zsd_integra_siagri.

*DELETE FROM zsdt0299.
*COMMIT WORK.
*EXIT.
*BREAK-POINT.
DATA: tb_receitas  TYPE zsde0070.

*******************************************************************************************
* TELA SELECAO
*******************************************************************************************
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
PARAMETERS: p_dtini TYPE datum MODIF ID t1 OBLIGATORY,
            p_dtfim TYPE datum MODIF ID t1 OBLIGATORY.
SELECTION-SCREEN END OF BLOCK b1.

TRY .

    zcl_integracao_agriq=>zif_integracao_agriq~get_instance(
       )->set_consultar_receita_siagri( EXPORTING i_data_inicio = p_dtini
                                                  i_data_fim    = p_dtfim
                                        IMPORTING e_receitas    = tb_receitas ).

  CATCH zcx_integracao INTO DATA(ex_integra).
    ex_integra->zif_error~published_erro( i_msgty = 'S' ). " i_msgty_display = 'E' ).

  CATCH zcx_error INTO DATA(ex_error).    "  "
    ex_error->zif_error~published_erro(   i_msgty = 'S' ). "i_msgty_display = 'E' ).

ENDTRY.
