*&---------------------------------------------------------------------*
*& Report  ZLES0086
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT zles0086.

*----------------------------------------------------------------------*
* Estruturas
*----------------------------------------------------------------------*

TABLES: zib_nfe_dist_ter,
        zib_cte_dist_cvl.

*----------------------------------------------------------------------*
* Tipos
*----------------------------------------------------------------------*

TYPES: BEGIN OF ty_saida,
         chave_nfe        TYPE zib_nfe_dist_ter-chave_nfe,
         prod_vlr_total_b TYPE zib_nfe_dist_itm-prod_vlr_total_b,
         tarifa           TYPE zib_cte_dist_cvl-valr_componente,
       END OF ty_saida,

       BEGIN OF ty_dados_nfe,
         chave_nfe        TYPE zib_nfe_dist_ter-chave_nfe,
         dt_saida         TYPE zib_nfe_dist_ter-dt_saida,
         prod_vlr_total_b TYPE zib_nfe_dist_itm-prod_vlr_total_b,
       END OF ty_dados_nfe,

       BEGIN OF ty_tarifa,
         n55_chave_acesso TYPE zib_cte_dist_n55-n55_chave_acesso,
         cd_chave_cte     TYPE zib_cte_dist_n55-cd_chave_cte,
         valr_componente  TYPE zib_cte_dist_cvl-valr_componente,
       END OF ty_tarifa.

*----------------------------------------------------------------------*
* Tabelas
*----------------------------------------------------------------------*

DATA: it_saida     TYPE TABLE OF ty_saida,
      it_dados_nfe TYPE TABLE OF ty_dados_nfe,
      it_tarifa    TYPE TABLE OF ty_tarifa,
      it_fcat      TYPE lvc_t_fcat.
*----------------------------------------------------------------------*
* Estrutura e Objetos ALV
*----------------------------------------------------------------------*
data: o_gui_alv_grid            TYPE REF TO cl_gui_alv_grid.

*----------------------------------------------------------------------*
* TELA DE SELEÇÃO
*----------------------------------------------------------------------*

SELECTION-SCREEN: BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.

SELECT-OPTIONS:  p_chave  FOR zib_nfe_dist_ter-chave_nfe  NO INTERVALS,
                 p_period FOR zib_nfe_dist_ter-dt_saida .

SELECTION-SCREEN: END OF BLOCK b1.


START-OF-SELECTION.
  PERFORM fm_start_of_selection.

END-OF-SELECTION.

  CALL SCREEN 0100.

*&---------------------------------------------------------------------*
*&      Form  FM_START_OF_SELECTION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*

FORM fm_start_of_selection .

  PERFORM fm_dados_seleciona.
  PERFORM fm_dados_processa.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FM_DADOS_SELECIONA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM fm_dados_seleciona .

  CLEAR: it_dados_nfe[], it_tarifa[].

  SELECT a~chave_nfe a~dt_saida b~prod_vlr_total_b
   INTO TABLE it_dados_nfe
    FROM zib_nfe_dist_ter AS a
    INNER JOIN zib_nfe_dist_itm AS b
    ON a~chave_nfe = b~chave_nfe
    WHERE a~chave_nfe IN p_chave
  AND a~dt_saida  IN p_period.

  IF sy-subrc = 0.

    SELECT a~n55_chave_acesso a~cd_chave_cte b~valr_componente
      INTO TABLE it_tarifa
      FROM zib_cte_dist_n55 AS a
      INNER JOIN zib_cte_dist_cvl AS b
      ON a~cd_chave_cte = b~cd_chave_cte
      FOR ALL ENTRIES IN it_dados_nfe
      WHERE n55_chave_acesso = it_dados_nfe-chave_nfe
    AND nome_componente IN ('Tarifa', 'TARIFA', 'tarifa').

    IF sy-subrc <> 0.
      CLEAR it_tarifa.
    ENDIF.

  ELSE.
    MESSAGE 'Dados não encontrados.' TYPE 'S' DISPLAY LIKE 'E'.
    LEAVE LIST-PROCESSING.
  ENDIF.

ENDFORM.

FORM fm_dados_processa.

  LOOP AT it_dados_nfe ASSIGNING FIELD-SYMBOL(<fs_dados_nfe>).
    APPEND INITIAL LINE TO it_saida ASSIGNING FIELD-SYMBOL(<fs_saida>).

    <fs_saida>-chave_nfe = <fs_dados_nfe>-chave_nfe.
    <fs_saida>-prod_vlr_total_b = <fs_dados_nfe>-prod_vlr_total_b.

    READ TABLE it_tarifa INTO DATA(wa_tarifa) WITH KEY n55_chave_acesso = <fs_dados_nfe>-chave_nfe.
    IF sy-subrc = 0.
      <fs_saida>-tarifa = wa_tarifa-valr_componente.
    ENDIF.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0100 OUTPUT.

  DATA fcode TYPE TABLE OF sy-ucomm.

  SET PF-STATUS 'PF0100' EXCLUDING fcode.
  SET TITLEBAR  'TB0100' WITH 'Processo de Indenização'.

  PERFORM fm_criar_objetos.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Form  FM_CRIAR_OBJETOS
*&---------------------------------------------------------------------*
FORM fm_criar_objetos .

  DATA: lva_data(22)  TYPE c,
        lva_status_op TYPE string.

  PERFORM fm_cria_fieldcat.

  CONCATENATE sy-datum+6(2) '.'  sy-datum+4(2) '.' sy-datum+0(4) INTO lva_data.

  IF zcl_screen=>zif_screen~set_criar_tela_padrao_report(
  EXPORTING
*     i_titulo  =
     i_filtros = VALUE zif_screen_linha_filtro_t( ( parametro = 'Data Posição' valor = lva_data ) )
   CHANGING
     alv = o_gui_alv_grid
   )
   EQ abap_true.

      CALL METHOD o_gui_alv_grid->set_table_for_first_display
        CHANGING
          it_outtab                     = it_saida
          it_fieldcatalog               = it_fcat
        EXCEPTIONS
          invalid_parameter_combination = 1
          program_error                 = 2
          too_many_lines                = 3
          OTHERS                        = 4.

  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FM_CRIA_FIELDCAT
*&---------------------------------------------------------------------*
FORM fm_cria_fieldcat .

  TYPES: lt_fieldcat_aux TYPE TABLE OF lvc_s_fcat WITH DEFAULT KEY.

  it_fcat = VALUE lt_fieldcat_aux(
  ( tabname = 'IT_SAIDA'  fieldname = 'CHAVE_NFE'
                          coltext = 'Chave NFe'
                          col_opt = 'X'
                          no_zero = ''
                          ref_table = 'ZIB_NFE_DIST_TER' )
  ( tabname = 'IT_SAIDA'  fieldname = 'PROD_VLR_TOTAL_B'
                          coltext = 'Preço Ton'
                          col_opt = 'X'
                          no_zero = ''
                          ref_table = 'ZIB_NFE_DIST_ITM' )
  ( tabname = 'IT_SAIDA'  fieldname = 'TARIFA'
                          coltext = 'Tarifa'
                          col_opt = 'X'
                          no_zero = 'X'
*                          ref_table = 'ZIB_CTE_DIST_CVL'
                          ref_table = 'ZIB_NFE_DIST_TER'
                          ref_field = 'PROD_VLR_TOTAL_B')
  ).

ENDFORM.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100_EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100_exit INPUT.
  LEAVE TO SCREEN 0.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.
  CASE sy-ucomm.
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.
  ENDCASE.
ENDMODULE.
