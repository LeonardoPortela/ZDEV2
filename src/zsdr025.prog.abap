*&--------------------------------------------------------------------&*
*&                         relatório modulo SD                        &*
*&--------------------------------------------------------------------&*
*& Projeto..: AMAGGI                                                  &*
*& Autor....: Anderson Oenning                                        &*
*& Data.....: 10/12/2021                                              &*
*& Descrição: Consultar dados da tabela ZSDT0256                      &*
*& Transação:                                                         &*
*&--------------------------------------------------------------------&*
*& Projeto  :                                                         &*
*& Código Espec.Funcional/Técnica:                                    &*
*&--------------------------------------------------------------------&*
*&                    Histórico de Modificações                       &*
*& Autor           Request      Data         Descrição                &*
*& ABAP                                                               &*
*&--------------------------------------------------------------------&*
REPORT zsdr025.

TABLES: zsdt0256, makt, vbap, kna1, mara, zib_cte_dist_ter.



TYPES: BEGIN OF ty_saida,
         data       TYPE zsdt0256-data,
         safra      TYPE zsdt0256-safra,
         cultura    TYPE zsdt0256-cultura,
         vbeln      TYPE zsdt0256-vbeln,
         posnr      TYPE zsdt0256-posnr,
         matnr      TYPE zsdt0256-matnr,
         werks      TYPE zsdt0256-werks,
         qtd_ov     TYPE zsdt0256-qtd_ov,
         qtd_sald   TYPE zsdt0256-qtd_sald,
         um_ov      TYPE zsdt0256-um_ov,
         matnr_mp   TYPE zsdt0256-matnr_mp,
         fator_mp   TYPE zsdt0256-fator_mp,
         qtd_cons   TYPE zsdt0256-qtd_cons,
         qtd_ac     TYPE zsdt0256-qtd_ac,
         data_atual TYPE zsdt0256-data_atual,
         hora_atual TYPE zsdt0256-hora_atual,
         name1      TYPE kna1-name1,
         ort01      TYPE kna1-ort01,
         regio      TYPE kna1-regio,
         arktx      TYPE vbap-arktx,
         maktx      TYPE makt-maktx,
         maktx_     TYPE makt-maktx,
         matkl      TYPE vbap-matkl,
         mtart      TYPE mara-mtart.
TYPES      END OF ty_saida.

TYPES: BEGIN OF ty_data,
         data TYPE zsdt0256-data.
TYPES END OF ty_data.



"Objetos
DATA: gob_custom_container        TYPE REF TO cl_gui_custom_container,
      gob_dd_document             TYPE REF TO cl_dd_document,
      gob_splitter_container_main TYPE REF TO cl_gui_splitter_container,
      gob_splitter_container_topo TYPE REF TO cl_gui_splitter_container,

      gob_gui_container_topo      TYPE REF TO cl_gui_container,
      gob_gui_container_filtro    TYPE REF TO cl_gui_container,
      gob_gui_container_logo      TYPE REF TO cl_gui_container,
      gob_gui_container_grid      TYPE REF TO cl_gui_container,
      gob_gui_picture             TYPE REF TO cl_gui_picture,
      git_fcat                    TYPE lvc_t_fcat,
      gob_gui_alv_grid            TYPE REF TO cl_gui_alv_grid,
      lines                       TYPE sy-tabix,
      wa_selected_rows            TYPE lvc_s_row,
      it_selected_rows            TYPE lvc_t_row.

" Classe
*CLASS lcl_event_receiver DEFINITION DEFERRED.
*DATA:  event_receiver   TYPE REF TO lcl_event_receiver.


DATA: it_saida TYPE TABLE OF ty_saida,
      it_t256  TYPE TABLE OF ty_data,
      w256     TYPE ty_data.

DATA: tp_safra TYPE  zsdt0192-tp_safra.

SELECTION-SCREEN: BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
SELECT-OPTIONS: p_werks  FOR zsdt0256-werks,
                p_safra  FOR zsdt0256-safra,
                p_mtart  FOR mara-mtart NO INTERVALS.
PARAMETER:     p_data   LIKE zsdt0192-data.
SELECTION-SCREEN: END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE text-002.
PARAMETERS: r_safra  RADIOBUTTON GROUP g1,
            r_safrin RADIOBUTTON GROUP g1,
            r_todos  RADIOBUTTON GROUP g1.
SELECTION-SCREEN END OF BLOCK b2.

INITIALIZATION.

  FREE: it_t256.
  SELECT data FROM zsdt0256 INTO TABLE it_t256 WHERE qtd_ac NE 0.
  IF sy-subrc EQ 0.
    SORT it_t256 DESCENDING BY data.
    CLEAR: w256.
    READ TABLE it_t256[] INTO w256 INDEX 1.
    IF w256 IS NOT INITIAL.
      p_data = w256-data.
    ENDIF.
  ENDIF.

START-OF-SELECTION.
  PERFORM fm_seleciona_dados.
*&---------------------------------------------------------------------*
*&      Form  FM_SELECIONA_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fm_seleciona_dados .

  IF r_safra IS NOT INITIAL.
    tp_safra = 1.
  ELSEIF r_safrin IS NOT INITIAL .
    tp_safra = 2.
  ENDIF.

  IF r_todos IS NOT INITIAL.

    SELECT DISTINCT a~data a~safra a~cultura a~vbeln a~posnr a~matnr a~werks a~qtd_ov
      a~qtd_sald a~um_ov a~matnr_mp a~fator_mp a~qtd_cons a~qtd_ac a~data_atual
      a~hora_atual b~arktx b~matkl c~name1 c~ort01 c~regio d~maktx f~mtart
    FROM zsdt0256 AS a
    LEFT JOIN vbap AS b ON b~vbeln EQ a~vbeln
    LEFT JOIN kna1 AS c ON c~kunnr EQ a~kunnr
    LEFT JOIN makt AS d ON d~matnr EQ a~matnr_mp
    LEFT JOIN makt AS e ON e~matnr EQ a~matnr
    LEFT JOIN mara AS f ON f~matnr EQ a~matnr
    INTO CORRESPONDING FIELDS OF TABLE it_saida
      WHERE a~data EQ p_data
        AND a~werks IN p_werks
        AND a~qtd_ac NE 0
        AND a~safra IN p_safra.

  ELSE.
    SELECT DISTINCT a~data a~safra a~cultura a~vbeln a~posnr a~matnr a~werks a~qtd_ov
      a~qtd_sald a~um_ov a~matnr_mp a~fator_mp a~qtd_cons a~qtd_ac a~data_atual
      a~hora_atual b~arktx b~matkl c~name1 c~ort01 c~regio f~mtart
    FROM zsdt0256 AS a
    LEFT JOIN vbap AS b ON b~vbeln EQ a~vbeln
    LEFT JOIN kna1 AS c ON c~kunnr EQ a~kunnr
    LEFT JOIN mara AS f ON f~matnr EQ a~matnr
    INTO CORRESPONDING FIELDS OF TABLE it_saida
      WHERE a~data EQ p_data
        AND a~werks IN p_werks
        AND a~safra IN p_safra
        AND a~qtd_ac NE 0
        AND a~tp_safra EQ tp_safra.
  ENDIF.

*  IF p_matkl IS NOT INITIAL.
*    DELETE it_saida WHERE matkl NOT IN p_matkl.
*  ENDIF.

  IF p_mtart IS NOT INITIAL.
    DELETE it_saida WHERE mtart NOT IN p_mtart.
  ENDIF.

  "Descrição do material.
  SELECT * FROM makt
    INTO TABLE @DATA(it_makt)
    FOR ALL ENTRIES IN @it_saida
    WHERE matnr EQ @it_saida-matnr.

  SELECT * FROM makt
  APPENDING TABLE it_makt
  FOR ALL ENTRIES IN it_saida
  WHERE matnr EQ it_saida-matnr_mp.

  IF it_makt IS NOT INITIAL.
    LOOP AT it_saida ASSIGNING FIELD-SYMBOL(<ls_saida>).
      READ TABLE it_makt INTO DATA(ws_makt) WITH KEY matnr = <ls_saida>-matnr.
      IF sy-subrc EQ 0.
        <ls_saida>-maktx = ws_makt-maktx.
      ENDIF.

      CLEAR: ws_makt.
      READ TABLE it_makt INTO ws_makt WITH KEY matnr = <ls_saida>-matnr_mp.
      IF sy-subrc EQ 0.
        <ls_saida>-maktx_ = ws_makt-maktx.
      ENDIF.
    ENDLOOP.

  ENDIF.

  IF it_saida IS NOT INITIAL.
    SORT it_saida BY data matnr_mp vbeln posnr matnr werks.
    DELETE ADJACENT DUPLICATES FROM it_saida COMPARING data matnr_mp vbeln posnr matnr werks.
  ENDIF.

*  IF it_zsdt0256 IS NOT INITIAL.
  CALL SCREEN 0100.
*  ENDIF.


ENDFORM.
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
  SET PF-STATUS 'ST0100'.
*  SET TITLEBAR 'xxx'.

  PERFORM fm_criar_objetos.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.


  CASE sy-ucomm.
    WHEN 'EXIT'.
      LEAVE TO SCREEN 0.

    WHEN OTHERS.
  ENDCASE.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Form  FM_CRIAR_OBJETOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fm_criar_objetos .


  DATA: lva_data(22) TYPE c,
        w_layout     TYPE lvc_s_layo.

  DATA: gs_variant  TYPE disvariant.
  gs_variant-report      = sy-repid.

  PERFORM fm_cria_fieldcat.


  CONCATENATE sy-datum+6(2) '.'  sy-datum+4(2) '.' sy-datum+0(4) INTO lva_data.

  IF zcl_screen=>zif_screen~set_criar_tela_padrao_report(
    EXPORTING
       i_titulo  = 'Relatório consumo de matéria prima em carteira'
       i_filtros = VALUE zif_screen_linha_filtro_t( ( parametro = 'Data Posição' valor = lva_data ) )
     CHANGING
       alv = gob_gui_alv_grid
     )
     EQ abap_true.


*    CREATE OBJECT event_receiver.
*    SET HANDLER event_receiver->hotspot_click  FOR gob_gui_alv_grid.
*    SET HANDLER event_receiver->get_ucomm  FOR gob_gui_alv_grid.

    w_layout-cwidth_opt = abap_true.
    w_layout-zebra      = 'X'.
    w_layout-sel_mode   = 'A'.
    w_layout-col_opt    = abap_true.



    CALL METHOD gob_gui_alv_grid->set_table_for_first_display
      EXPORTING
        is_layout                     = w_layout
        i_save                        = 'A'
        is_variant                    = gs_variant
      CHANGING
        it_outtab                     = it_saida
        it_fieldcatalog               = git_fcat
*       IT_SORT                       =
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
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fm_cria_fieldcat .

  TYPES: lit_fieldcat_aux TYPE TABLE OF lvc_s_fcat WITH DEFAULT KEY.
  git_fcat = VALUE lit_fieldcat_aux(
( fieldname ='DATA          '      coltext = 'Data                 '    ref_table = 'ZSDT0256'     ref_field = 'DATA          '        no_zero = '' )
( fieldname ='SAFRA         '      coltext = 'Ano Safra            '    ref_table = 'ZSDT0256'     ref_field = 'SAFRA         '        no_zero = '' )
( fieldname ='CULTURA       '      coltext = 'Cultura              '    ref_table = 'ZSDT0256'     ref_field = 'CULTURA       '        no_zero = '' )
( fieldname ='VBELN         '      coltext = 'Ordem de venda       '    ref_table = 'ZSDT0256'     ref_field = 'VBELN         '        no_zero = '' )
( fieldname ='POSNR         '      coltext = 'Item da OV           '    ref_table = 'ZSDT0256'     ref_field = 'POSNR         '        no_zero = '' )
( fieldname ='MATNR         '      coltext = 'Material             '    ref_table = 'ZSDT0256'     ref_field = 'MATNR         '        no_zero = '' )
( fieldname ='MAKTX         '      coltext = 'Descrição material    '    ref_table = 'MAKT    '     ref_field = 'MAKTX         '        no_zero = '' )
( fieldname ='WERKS         '      coltext = 'Centro Fornec.       '    ref_table = 'ZSDT0256'     ref_field = 'WERKS         '        no_zero = '' )
( fieldname ='QTD_OV        '      coltext = 'Qtd OV               '    ref_table = 'ZSDT0256'     ref_field = 'QTD_OV        '        no_zero = '' )
( fieldname ='QTD_SALD      '      coltext = 'Saldo OV             '    ref_table = 'ZSDT0256'     ref_field = 'QTD_SALD      '        no_zero = '' )
( fieldname ='UM_OV         '      coltext = 'Unidade              '    ref_table = 'ZSDT0256'     ref_field = 'UM_OV         '        no_zero = '' )
( fieldname ='MATNR_MP      '      coltext = 'Matéria Prima        '    ref_table = 'ZSDT0256'     ref_field = 'MATNR_MP      '        no_zero = '' )
( fieldname ='FATOR_MP      '      coltext = 'Fator materia prima  '    ref_table = 'ZSDT0256'     ref_field = 'FATOR_MP      '        no_zero = '' )
( fieldname ='QTD_CONS      '      coltext = 'Qtd.Consumido        '    ref_table = 'ZSDT0256'     ref_field = 'QTD_CONS      '        no_zero = '' )
( fieldname ='QTD_AC        '      coltext = 'Qtd.á Consumir       '    ref_table = 'ZSDT0256'     ref_field = 'QTD_AC        '        no_zero = '' )
( fieldname ='DATA_ATUAL    '      coltext = 'Data atualização     '    ref_table = 'ZSDT0256'     ref_field = 'DATA_ATUAL    '        no_zero = '' )
( fieldname ='HORA_ATUAL    '      coltext = 'Hora atualização     '    ref_table = 'ZSDT0256'     ref_field = 'HORA_ATUAL    '        no_zero = '' )
( fieldname ='NAME1         '      coltext = 'Cliente/fornecedor   '    ref_table = 'KNA1    '     ref_field = 'NAME1         '        no_zero = '' )
( fieldname ='ORT01         '      coltext = 'Cidade entrega       '    ref_table = 'KNA1    '     ref_field = 'ORT01         '        no_zero = '' )
( fieldname ='REGIO         '      coltext = 'Estado               '    ref_table = 'KNA1    '     ref_field = 'REGIO         '        no_zero = '' )
*( fieldname ='ARKTX         '      coltext = 'Desc.item ov         '    ref_table = 'VBAP    '     ref_field = 'ARKTX         '        no_zero = '' )
( fieldname ='MAKTX_        '      coltext = 'Descrição materia prima'  ref_table = 'MAKT    '     ref_field = 'MAKTX         '        no_zero = '' )
( fieldname ='MATKL         '      coltext = 'Grupo mercadoria     '    ref_table = 'MATKL   '     ref_field = 'MAKTX         '        no_zero = '' )
( fieldname ='MTART         '      coltext = 'Tipo material        '    ref_table = 'MARA    '     ref_field = 'MTART         '        no_zero = '' )
).
ENDFORM.
