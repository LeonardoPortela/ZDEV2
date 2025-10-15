*&---------------------------------------------------------------------*
*& Include          ZMMR189_MAIN
*&---------------------------------------------------------------------*
*----------------------------------------------------------------------*
* DOCKER                                                  *
*----------------------------------------------------------------------*
FORM docker.
  CREATE OBJECT container_main
    EXPORTING
      container_name = 'CONTAINER'
      lifetime       = container_main->lifetime_dynpro.

* Cria Splitter Container
  CREATE OBJECT painel_control
    EXPORTING
      parent  = container_main
      rows    = 1
      columns = 1
      align   = 70.

* Exibe Painel 1
  CALL METHOD painel_control->get_container
    EXPORTING
      row       = 2
      column    = 1
    RECEIVING
      container = painel1.

** Exibe Painel 2 "hABILITA E DIVIDE A TELA PARA 2 PARTE ABAIXO DO PAINEL 1
*    CALL METHOD painel_control->get_container
*      EXPORTING
*        row       = 2
*        column    = 1
*      RECEIVING
*        container = painel_2.

ENDFORM.

*FORM docker_p1.
*
*  TRY.
*      cl_salv_table=>factory(
*        EXPORTING
*      r_container = painel2
*      container_name = 'CONTAINER'
*        IMPORTING
*          r_salv_table   = p01_gr_table
*        CHANGING
*          t_table        = it_saida_p01[] ).
*    CATCH cx_salv_msg.                                  "#EC NO_HANDLER
*  ENDTRY.
*
*ENDFORM.


FORM docker_p2.
  TRY.
      cl_salv_table=>factory(
        EXPORTING
      r_container = painel1
      container_name = 'CONTAINER'
        IMPORTING
          r_salv_table   = p21_gr_table
        CHANGING
          t_table        = it_dados_p21[] ).
    CATCH cx_salv_msg.                                  "#EC NO_HANDLER
  ENDTRY.
ENDFORM.


*FORM docker_p22.
*  TRY.
*      cl_salv_table=>factory(
*        EXPORTING
*      r_container = painel1
*      container_name = 'CONTAINER'
*        IMPORTING
*          r_salv_table   = p22_gr_table
*        CHANGING
*          t_table        = it_dados_p22[] ).
*    CATCH cx_salv_msg.                                  "#EC NO_HANDLER
*  ENDTRY.
*
*  ENDFORM.
