*&---------------------------------------------------------------------*
*& Report  ZDCO_ENTRADAS
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  zdco_entradas NO STANDARD PAGE HEADING  .

*----------------------------------------------------------------------*
* Tabelas                                                              *
*----------------------------------------------------------------------*
TABLES: zdco_produtor, zdco_nota, zdco_nota_vinc.

*----------------------------------------------------------------------*
* Tipos                                                                *
*----------------------------------------------------------------------*
TYPE-POOLS: slis,
            kkblo,
            stree.

*----------------------------------------------------------------------*
* Tabelas Internas (ALV)                                               *
*----------------------------------------------------------------------*
DATA: BEGIN OF wa_dco,
        nu_dco          LIKE zdco_produtor-nu_dco,
        nr_dco          LIKE zdco_produtor-nr_dco,
        dt_lancamento   LIKE zdco_produtor-dt_lancamento,
        qt_material     LIKE zdco_produtor-qt_material,
        id_fornecedor   LIKE zdco_produtor-id_fornecedor,
        cd_material     LIKE zdco_produtor-cd_material,
        cd_centro       LIKE zdco_produtor-cd_centro,
        cd_safra        LIKE zdco_produtor-cd_safra,
        cd_tipo_leilao  LIKE zdco_produtor-cd_tipo_leilao,
        doc_venda       LIKE zdco_produtor-vbeln,
        qt_entregue     LIKE zdco_produtor-qt_entregue,
        qt_remessa      LIKE zdco_produtor-qt_remessa,
        nome_fornecedor TYPE name1,
        nome_deposito   TYPE lgobe,
        nome_centro     TYPE name1,
        nome_material   TYPE maktx,
        ds_tipo_leilao  TYPE zds_tipo_leilao,
        saldo_entregue  TYPE zqt_material,
        saldo_remessa   TYPE zqt_material.
DATA: END   OF wa_dco.

DATA: BEGIN OF wa_disp.
        INCLUDE STRUCTURE zdco_nota.
DATA: END OF wa_disp.

DATA: BEGIN OF wa_vinc.
        INCLUDE STRUCTURE zdco_nota_vinc.
DATA: END OF wa_vinc.

DATA: it_fieldcat        TYPE slis_t_fieldcat_alv,                   "Estrutura de saida
      it_event           TYPE slis_t_event       WITH HEADER LINE,   "Eventos
      it_header          TYPE kkblo_t_listheader WITH HEADER LINE,   "Cabeçalho
      vg_layout          TYPE slis_layout_alv,   "Layout do alv
      vg_ucomm           TYPE stree_ucomm,
      it_dco             LIKE STANDARD TABLE OF wa_dco,
      vg_top             TYPE i VALUE 1,
      vg_alterou         TYPE c VALUE IS INITIAL,
      it_disp            LIKE STANDARD TABLE OF wa_disp,
      it_disp2           LIKE STANDARD TABLE OF wa_disp,
      it_vinc            LIKE STANDARD TABLE OF wa_vinc,
      it_vinc2           LIKE STANDARD TABLE OF wa_vinc,
      it_desvinculados   LIKE STANDARD TABLE OF wa_vinc.

*----------------------------------------------------------------------*
* Telas de Inclusão / Alteração / Exclusão
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-s01.
SELECT-OPTIONS: p_nu_dco  FOR zdco_produtor-nu_dco,
                p_nr_dco  FOR zdco_produtor-nr_dco,
                p_dt_lct  FOR zdco_produtor-dt_lancamento,
                p_id_for  FOR zdco_produtor-id_fornecedor,
                p_cd_mat  FOR zdco_produtor-cd_material ,
                p_cd_ctr  FOR zdco_produtor-cd_centro,
                p_cd_sf   FOR zdco_produtor-cd_safra,
                p_tp_lei  FOR zdco_produtor-cd_tipo_leilao,
                p_vbeln   FOR zdco_produtor-vbeln.
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE text-s02.
PARAMETERS: p_oplis  RADIOBUTTON GROUP arq DEFAULT 'X',
            p_opgra  RADIOBUTTON GROUP arq.
SELECTION-SCREEN END OF BLOCK b2.


INITIALIZATION.

START-OF-SELECTION.

  IF p_oplis IS NOT INITIAL.

    " Busco dados do primeiro nivel da estrutura de DRE
    PERFORM f_seleciona_dados.

    IF NOT it_dco IS INITIAL.
*    SET TITLEBAR  '001'.
*    "Montar estruduta de ALV
*    PERFORM f_estrutura_alv.
*    "Executa ALV
*    PERFORM f_motra_alv.
      PERFORM f_monta_lista.
    ENDIF.

  ELSE.
    PERFORM f_monta_grafico.
  ENDIF.


TOP-OF-PAGE.

  IF vg_top EQ 1.
    ULINE.
    FORMAT COLOR COL_HEADING INTENSIFIED ON.
    WRITE:/001 'Código',
           013 'Número',
           028 'Dt. Lanct',
           040 'Fornecedor',
           076 'Centro',
           109 'Depósito',
           127 'Doc. Venda',
           139 'Material',
           186 '  Quantidade',
           198 '    Entregue',
           210 '     Remessa',
           222 '  Saldo Ent.',
           234 '  Saldo Rem.'.
    ULINE.
  ENDIF.


AT LINE-SELECTION.

  DATA: vg_field TYPE string,
        vg_value TYPE string.

  READ CURRENT LINE.

  GET CURSOR FIELD vg_field VALUE vg_value.

  CASE vg_field.
    WHEN 'WA_DCO-NU_DCO'.

      CALL FUNCTION 'ZENQUEUE_ZDCO_NOTA_ENTRADA'
        EXPORTING
          mode_zdco_vinc = 'X'
          mandt          = sy-mandt
          nu_dco         = wa_dco-nu_dco
          _scope         = '2'
          _wait          = ' '
          _collect       = ' '
        EXCEPTIONS
          foreign_lock   = 1
          system_failure = 2
          OTHERS         = 3.

      CASE sy-subrc.
        WHEN 1.

          DATA garg LIKE seqg3-garg.
          DATA: BEGIN OF wa_enq.
                  INCLUDE STRUCTURE seqg7.
          DATA: END OF wa_enq.
          DATA: enq LIKE STANDARD TABLE OF wa_enq,
                msg TYPE c LENGTH 100.

          CONCATENATE sy-mandt wa_dco-nu_dco INTO garg.

          CALL FUNCTION 'ENQUE_READ2'
            EXPORTING
              gname  = 'ZDCO_NF_ENTRADA'
              garg   = garg
              guname = '*'
            TABLES
              enq    = enq.

          READ TABLE enq INTO wa_enq WITH KEY gname = 'ZDCO_NF_ENTRADA'.

          CONCATENATE 'Registro bloqueado com o usuário' wa_enq-guname INTO msg SEPARATED BY space.

          MESSAGE msg TYPE 'E'.
        WHEN 2 OR 3.
          MESSAGE 'Erro em bloqueio!' TYPE 'E'.
      ENDCASE.

      PERFORM consulta_notas_vinculadas.
      CALL SCREEN 0001.
      LEAVE SCREEN.
    WHEN 'WA_DCO-NR_DCO'.

      CALL FUNCTION 'VIEW_MAINTENANCE_CALL'
        EXPORTING
          action              = 'S'
          view_name           = 'ZDCO_PRODUTOR'
          check_ddic_mainflag = 'X'                         "MF 081100
        EXCEPTIONS
          foreign_lock        = 2
          no_tvdir_entry      = 8.

      IF sy-subrc NE 0.
        MESSAGE 'Tela de cadastro de DCO não pode ser chamada!' TYPE 'E'.
      ENDIF.

    WHEN 'WA_DCO-DOC_VENDA'.
      IF wa_dco-doc_venda IS NOT INITIAL.
        SET PARAMETER ID 'AUN' FIELD wa_dco-doc_venda.
        CALL TRANSACTION 'VA03' AND SKIP FIRST SCREEN.
      ENDIF.
  ENDCASE.

*&---------------------------------------------------------------------*
*&      Form  F_SELECIONA_DADOS
*&---------------------------------------------------------------------*
*       Seleciona dados de DCO
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_seleciona_dados .

  DATA it_dco_aux LIKE STANDARD TABLE OF wa_dco.

  CLEAR: it_dco,
         it_dco_aux,
         wa_dco.

  SELECT zp~nu_dco
         zp~nr_dco
         zp~dt_lancamento
         zp~qt_material
         zp~id_fornecedor
         zp~cd_material
         zp~cd_centro
         zp~cd_safra
         zp~cd_tipo_leilao
         zp~vbeln
         zp~qt_entregue
         zp~qt_remessa
         lf~name1
         t1~lgobe
         t0~name1
         mk~maktx
         tl~ds_tipo_leilao
    INTO TABLE it_dco_aux
    FROM zdco_produtor    AS zp
    LEFT JOIN makt AS mk ON mk~spras EQ sy-langu AND mk~matnr EQ zp~cd_material
    LEFT JOIN zdco_tipo_leilao AS tl ON tl~cd_tipo_leilao EQ zp~cd_tipo_leilao
    LEFT JOIN lfa1 AS lf ON lf~lifnr EQ zp~id_fornecedor
    LEFT JOIN t001w AS t0 ON t0~werks EQ zp~cd_centro
    LEFT JOIN t001l AS t1 ON t1~werks EQ zp~cd_centro AND t1~lgort EQ zp~cd_safra
   WHERE zp~nu_dco IN p_nu_dco
     AND zp~nr_dco IN p_nr_dco
     AND zp~dt_lancamento IN p_dt_lct
     AND zp~id_fornecedor IN p_id_for
     AND zp~cd_material IN p_cd_mat
     AND zp~cd_centro IN p_cd_ctr
     AND zp~cd_safra IN p_cd_sf
     AND zp~cd_tipo_leilao IN p_tp_lei
     AND zp~vbeln IN p_vbeln.

  LOOP AT it_dco_aux INTO wa_dco.
    CONCATENATE wa_dco-nr_dco(2) '.' wa_dco-nr_dco+2(3) '.' wa_dco-nr_dco+5(4) '-' wa_dco-nr_dco+9(4) INTO wa_dco-nr_dco.
    wa_dco-saldo_entregue = wa_dco-qt_material - wa_dco-qt_entregue.
    wa_dco-saldo_remessa  = wa_dco-qt_material - wa_dco-qt_remessa.
    APPEND wa_dco TO it_dco.
  ENDLOOP.

  SORT it_dco BY nu_dco.

ENDFORM.                    " F_SELECIONA_DADOS

*&---------------------------------------------------------------------*                                                                      *
*&      Form  f_fieldcat                                               *
*&---------------------------------------------------------------------*
* Preenche a tabela fieldcat                                           *
*----------------------------------------------------------------------*
* p_cont   -> Posição do campo                                         *
* p_key    -> campo chave                                              *
* p_tab    -> tabela interna                                           *
* p_field  -> campo da tabela interna                                  *
* p_desc   -> Descrição do campo                                       *
* p_tam    -> Tamanho do campo de saída                                *
* p_qtde   -> É um campo de to tipo QUAN                               *
* p_fix    -> Congelar a coluna                                        *
* p_just-> -> Alinhamento (R)ight (L)eft (C)ent                        *
*----------------------------------------------------------------------*
FORM f_fieldcat USING p_cont p_key  p_tab  p_field p_desc
                      p_tam  p_qtde p_fix  p_just p_hot
             CHANGING p_fieldcat TYPE slis_t_fieldcat_alv.

* Tabela interna local
  DATA: tl_fieldcat TYPE slis_t_fieldcat_alv WITH HEADER LINE.

  tl_fieldcat-col_pos    = p_cont. "Posição
  tl_fieldcat-key        = p_key.  "
  tl_fieldcat-tabname    = p_tab.  "Tabela interna
  tl_fieldcat-fieldname  = p_field."Campo
  tl_fieldcat-seltext_l  = p_desc. "Descrição longa
  tl_fieldcat-seltext_m  = p_desc. "Descrição media
  tl_fieldcat-seltext_s  = p_desc. "Descrição pequena
  tl_fieldcat-outputlen  = p_tam.  "Tamanho
  tl_fieldcat-quantity   = p_qtde. "Campo quantidade
  tl_fieldcat-fix_column = p_fix.  "Fixar coluna
  tl_fieldcat-just       = p_just. "Alinhar
  tl_fieldcat-hotspot    = p_hot.  "Clique chama evento
  APPEND tl_fieldcat TO p_fieldcat.

ENDFORM.                    " f_fieldcatJ1BNFDOC

*&---------------------------------------------------------------------*
*&      Form  F_ESTRUTURA_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_estrutura_alv .

  PERFORM f_fieldcat USING:
        '00' '' 'IT_DCO' 'NU_DCO'          'Código'     10  ''  ''      '' 'X'  CHANGING it_fieldcat,
        '01' '' 'IT_DCO' 'NR_DCO'          'Número'     15  ''  ''      '' ''   CHANGING it_fieldcat,
        '02' '' 'IT_DCO' 'DT_LANCAMENTO'   'Dt. Lanct'  10  ''  ''      '' ''   CHANGING it_fieldcat,
        '03' '' 'IT_DCO' 'NOME_FORNECEDOR' 'Fornecedor' 35  ''  ''      '' ''   CHANGING it_fieldcat,
        '04' '' 'IT_DCO' 'CD_CENTRO'       'Centro'     06  ''  ''      '' ''   CHANGING it_fieldcat,
        '05' '' 'IT_DCO' 'CD_SAFRA'        'Depósito'   08  ''  ''      '' ''   CHANGING it_fieldcat,
        '06' '' 'IT_DCO' 'DOC_VENDA'       'Doc. Venda' 10  ''  ''      '' 'X'  CHANGING it_fieldcat,
        '07' '' 'IT_DCO' 'NOME_MATERIAL'   'Material'   45  ''  ''      '' ''   CHANGING it_fieldcat,
        '08' '' 'IT_DCO' 'QT_MATERIAL'     'Quantidade' 10  ''  ''      '' ''   CHANGING it_fieldcat,
        '09' '' 'IT_DCO' 'QT_ENTREGUE'     'Entregue'   10  ''  ''      '' ''   CHANGING it_fieldcat,
        '10' '' 'IT_DCO' 'QT_REMESSA'      'Remessa'    10  ''  ''      '' ''   CHANGING it_fieldcat,
        '11' '' 'IT_DCO' 'SALDO_ENTREGUE'  'Saldo Ent.' 10  ''  ''      '' ''   CHANGING it_fieldcat,
        '12' '' 'IT_DCO' 'SALDO_REMESSA'   'Saldo Rem.' 10  ''  ''      '' ''   CHANGING it_fieldcat.

ENDFORM.                    " F_ESTRUTURA_ALV

*&---------------------------------------------------------------------*
*&      Form  F_MOTRA_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_motra_alv .

* Variavel Local
  DATA: vl_repid LIKE sy-repid.

  vl_repid = sy-repid.
  it_event-name = slis_ev_top_of_page.
  it_event-form = slis_ev_top_of_page.
  APPEND it_event.

* Determinar a tabela de cores
  vg_layout-zebra               = 'X'.

* Função para exibir o ALV
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program       = vl_repid
      i_callback_pf_status_set = 'TELA'
      i_callback_user_command  = 'USER_COMMAND'
      is_layout                = vg_layout
      it_fieldcat              = it_fieldcat[]
      i_default                = 'A'
      i_save                   = 'A'
    TABLES
      t_outtab                 = it_dco
    EXCEPTIONS
      program_error            = 1
      OTHERS                   = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
    WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.                    " F_MOTRA_ALV


*&---------------------------------------------------------------------*
*&      Form  USER_COMMAND
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM user_command USING p_ucomm LIKE sy-ucomm
      p_field TYPE slis_selfield.

  CLEAR: wa_dco.

  READ TABLE it_dco INTO wa_dco INDEX p_field-tabindex.

  IF p_field-fieldname = 'NU_DCO'.
    PERFORM consulta_notas_vinculadas.
    CALL SCREEN 0001.
  ENDIF.

  IF p_field-fieldname = 'DOC_VENDA'.

    IF wa_dco-doc_venda IS NOT INITIAL.

      SET PARAMETER ID 'AUN' FIELD wa_dco-doc_venda.
      CALL TRANSACTION 'VA03' AND SKIP FIRST SCREEN.

    ENDIF.

  ENDIF.

ENDFORM.                    " USER_COMMAND

INCLUDE zdco_entradas_user_command_i01.


*&---------------------------------------------------------------------*
*&      Form  F_MONTA_LISTA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_monta_lista .

  SET TITLEBAR 'NAME'.

  NEW-PAGE LINE-SIZE 246
           LINE-COUNT 20.

  LOOP AT it_dco INTO wa_dco.

    WRITE: /001 wa_dco-nu_dco HOTSPOT COLOR = 2,
            013 wa_dco-nr_dco HOTSPOT COLOR = 2,
            028 wa_dco-dt_lancamento,
            040 wa_dco-nome_fornecedor,
            076 wa_dco-nome_centro,
            109 wa_dco-nome_deposito,
            127 wa_dco-doc_venda HOTSPOT COLOR = 2,
            139 wa_dco-nome_material,
            186 wa_dco-qt_material,
            198 wa_dco-qt_entregue,
            210 wa_dco-qt_remessa,
            222 wa_dco-saldo_entregue,
            234 wa_dco-saldo_remessa.

    HIDE: wa_dco-nu_dco, wa_dco-nr_dco, wa_dco-dt_lancamento, wa_dco-nome_fornecedor, wa_dco-nome_centro,
          wa_dco-nome_deposito, wa_dco-doc_venda, wa_dco-nome_material, wa_dco-qt_material, wa_dco-qt_entregue,
          wa_dco-qt_remessa, wa_dco-saldo_entregue, wa_dco-saldo_remessa, wa_dco-id_fornecedor, wa_dco-cd_material,
          wa_dco-cd_centro.

  ENDLOOP.

ENDFORM.                    " F_MONTA_LISTA

*&---------------------------------------------------------------------*
*&      Form  CONSULTA_NOTAS_VINCULADAS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM consulta_notas_vinculadas .

  SELECT nf~docnum ni~itmnum nf~docdat nf~series nf~nfnum nf~branch ni~menge NF~NFENUM
    INTO CORRESPONDING FIELDS OF TABLE it_vinc2
    FROM j_1bnfdoc AS nf
   INNER JOIN j_1bnflin AS ni ON ni~docnum EQ nf~docnum AND ni~matnr EQ wa_dco-cd_material
   INNER JOIN zdco_nf_entrada AS NE ON ne~nu_dco EQ wa_dco-nu_dco AND ne~docnum EQ nf~docnum
   WHERE nf~branch EQ wa_dco-cd_centro.

  CLEAR it_vinc.

  LOOP AT it_vinc2 INTO wa_vinc.

*** Modificação - Eduardo Ruttkowski Tavares - 12.09.2013 >>> INI
* CH  109593 - Ajuste_Z_1BNFE_MONITOR_F32_ZSDI0001
    IF WA_vinc-NFNUM EQ '000000'.
        WA_VINC-NFNUM     = WA_vinc-NFENUM.
    ENDIF.
*** Modificação - Eduardo Ruttkowski Tavares - 12.09.2013 >>> END
    CONCATENATE wa_vinc-docdat+6(2) '.' wa_vinc-docdat+4(2) '.' wa_vinc-docdat(4) INTO wa_vinc-docdattxt.
    APPEND wa_vinc TO it_vinc.
  ENDLOOP.

ENDFORM.                    " CONSULTA_NOTAS_VINCULADAS

*&SPWIZARD: DECLARATION OF TABLECONTROL 'TABDISP' ITSELF
CONTROLS: tabdisp TYPE TABLEVIEW USING SCREEN 0001.

*&SPWIZARD: LINES OF TABLECONTROL 'TABDISP'
DATA:     g_tabdisp_lines  LIKE sy-loopc.

DATA:     ok_code LIKE sy-ucomm.

*&SPWIZARD: OUTPUT MODULE FOR TC 'TABDISP'. DO NOT CHANGE THIS LINE!
*&SPWIZARD: UPDATE LINES FOR EQUIVALENT SCROLLBAR
MODULE tabdisp_change_tc_attr OUTPUT.
  DESCRIBE TABLE it_disp LINES tabdisp-lines.
ENDMODULE.                    "TABDISP_CHANGE_TC_ATTR OUTPUT

*&SPWIZARD: INPUT MODUL FOR TC 'TABDISP'. DO NOT CHANGE THIS LINE!
*&SPWIZARD: MARK TABLE
MODULE tabdisp_mark INPUT.
  DATA: g_tabdisp_wa2 LIKE LINE OF it_disp.
  IF tabdisp-line_sel_mode = 1
  AND zdco_nota-mark = 'X'.
    LOOP AT it_disp INTO g_tabdisp_wa2
      WHERE mark = 'X'.
      g_tabdisp_wa2-mark = ''.
      MODIFY it_disp
        FROM g_tabdisp_wa2
        TRANSPORTING mark.
    ENDLOOP.
  ENDIF.
  MODIFY it_disp
    FROM zdco_nota
    INDEX tabdisp-current_line
    TRANSPORTING mark.
ENDMODULE.                    "TABDISP_MARK INPUT

*&SPWIZARD: INPUT MODULE FOR TC 'TABDISP'. DO NOT CHANGE THIS LINE!
*&SPWIZARD: PROCESS USER COMMAND
MODULE tabdisp_user_command INPUT.
  ok_code = sy-ucomm.
  PERFORM user_ok_tc USING    'TABDISP'
                              'IT_DISP'
                              'MARK'
                     CHANGING ok_code.
  sy-ucomm = ok_code.
ENDMODULE.                    "TABDISP_USER_COMMAND INPUT

*----------------------------------------------------------------------*
*   INCLUDE TABLECONTROL_FORMS                                         *
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  USER_OK_TC                                               *
*&---------------------------------------------------------------------*
FORM user_ok_tc USING    p_tc_name TYPE dynfnam
                         p_table_name
                         p_mark_name
                CHANGING p_ok      LIKE sy-ucomm.

*&SPWIZARD: BEGIN OF LOCAL DATA----------------------------------------*
  DATA: l_ok              TYPE sy-ucomm,
        l_offset          TYPE i.
*&SPWIZARD: END OF LOCAL DATA------------------------------------------*

*&SPWIZARD: Table control specific operations                          *
*&SPWIZARD: evaluate TC name and operations                            *
  SEARCH p_ok FOR p_tc_name.
  IF sy-subrc <> 0.
    EXIT.
  ENDIF.
  l_offset = STRLEN( p_tc_name ) + 1.
  l_ok = p_ok+l_offset.
*&SPWIZARD: execute general and TC specific operations                 *
  CASE l_ok.
    WHEN 'INSR'.                      "insert row
      PERFORM fcode_insert_row USING    p_tc_name
                                        p_table_name.
      CLEAR p_ok.

    WHEN 'DELE'.                      "delete row
      PERFORM fcode_delete_row USING    p_tc_name
                                        p_table_name
                                        p_mark_name.
      CLEAR p_ok.

    WHEN 'P--' OR                     "top of list
         'P-'  OR                     "previous page
         'P+'  OR                     "next page
         'P++'.                       "bottom of list
      PERFORM compute_scrolling_in_tc USING p_tc_name
                                            l_ok.
      CLEAR p_ok.
*     WHEN 'L--'.                       "total left
*       PERFORM FCODE_TOTAL_LEFT USING P_TC_NAME.
*
*     WHEN 'L-'.                        "column left
*       PERFORM FCODE_COLUMN_LEFT USING P_TC_NAME.
*
*     WHEN 'R+'.                        "column right
*       PERFORM FCODE_COLUMN_RIGHT USING P_TC_NAME.
*
*     WHEN 'R++'.                       "total right
*       PERFORM FCODE_TOTAL_RIGHT USING P_TC_NAME.
*
    WHEN 'MARK'.                      "mark all filled lines
      PERFORM fcode_tc_mark_lines USING p_tc_name
                                        p_table_name
                                        p_mark_name   .
      CLEAR p_ok.

    WHEN 'DMRK'.                      "demark all filled lines
      PERFORM fcode_tc_demark_lines USING p_tc_name
                                          p_table_name
                                          p_mark_name .
      CLEAR p_ok.

*     WHEN 'SASCEND'   OR
*          'SDESCEND'.                  "sort column
*       PERFORM FCODE_SORT_TC USING P_TC_NAME
*                                   l_ok.

  ENDCASE.

ENDFORM.                              " USER_OK_TC

*&---------------------------------------------------------------------*
*&      Form  FCODE_INSERT_ROW                                         *
*&---------------------------------------------------------------------*
FORM fcode_insert_row
              USING    p_tc_name           TYPE dynfnam
                       p_table_name             .

*&SPWIZARD: BEGIN OF LOCAL DATA----------------------------------------*
  DATA l_lines_name       LIKE feld-name.
  DATA l_selline          LIKE sy-stepl.
  DATA l_lastline         TYPE i.
  DATA l_line             TYPE i.
  DATA l_table_name       LIKE feld-name.
  FIELD-SYMBOLS <tc>                 TYPE cxtab_control.
  FIELD-SYMBOLS <table>              TYPE STANDARD TABLE.
  FIELD-SYMBOLS <lines>              TYPE i.
*&SPWIZARD: END OF LOCAL DATA------------------------------------------*

  ASSIGN (p_tc_name) TO <tc>.

*&SPWIZARD: get the table, which belongs to the tc                     *
  CONCATENATE p_table_name '[]' INTO l_table_name. "table body
  ASSIGN (l_table_name) TO <table>.                "not headerline

*&SPWIZARD: get looplines of TableControl                              *
  CONCATENATE 'G_' p_tc_name '_LINES' INTO l_lines_name.
  ASSIGN (l_lines_name) TO <lines>.

*&SPWIZARD: get current line                                           *
  GET CURSOR LINE l_selline.
  IF sy-subrc <> 0.                   " append line to table
    l_selline = <tc>-lines + 1.
*&SPWIZARD: set top line                                               *
    IF l_selline > <lines>.
      <tc>-top_line = l_selline - <lines> + 1 .
    ELSE.
      <tc>-top_line = 1.
    ENDIF.
  ELSE.                               " insert line into table
    l_selline = <tc>-top_line + l_selline - 1.
    l_lastline = <tc>-top_line + <lines> - 1.
  ENDIF.
*&SPWIZARD: set new cursor line                                        *
  l_line = l_selline - <tc>-top_line + 1.

*&SPWIZARD: insert initial line                                        *
  INSERT INITIAL LINE INTO <table> INDEX l_selline.
  <tc>-lines = <tc>-lines + 1.
*&SPWIZARD: set cursor                                                 *
  SET CURSOR LINE l_line.

ENDFORM.                              " FCODE_INSERT_ROW

*&---------------------------------------------------------------------*
*&      Form  FCODE_DELETE_ROW                                         *
*&---------------------------------------------------------------------*
FORM fcode_delete_row
              USING    p_tc_name           TYPE dynfnam
                       p_table_name
                       p_mark_name   .

*&SPWIZARD: BEGIN OF LOCAL DATA----------------------------------------*
  DATA l_table_name       LIKE feld-name.

  FIELD-SYMBOLS <tc>         TYPE cxtab_control.
  FIELD-SYMBOLS <table>      TYPE STANDARD TABLE.
  FIELD-SYMBOLS <wa>.
  FIELD-SYMBOLS <mark_field>.
*&SPWIZARD: END OF LOCAL DATA------------------------------------------*

  ASSIGN (p_tc_name) TO <tc>.

*&SPWIZARD: get the table, which belongs to the tc                     *
  CONCATENATE p_table_name '[]' INTO l_table_name. "table body
  ASSIGN (l_table_name) TO <table>.                "not headerline

*&SPWIZARD: delete marked lines                                        *
  DESCRIBE TABLE <table> LINES <tc>-lines.

  LOOP AT <table> ASSIGNING <wa>.

*&SPWIZARD: access to the component 'FLAG' of the table header         *
    ASSIGN COMPONENT p_mark_name OF STRUCTURE <wa> TO <mark_field>.

    IF <mark_field> = 'X'.
      DELETE <table> INDEX syst-tabix.
      IF sy-subrc = 0.
        <tc>-lines = <tc>-lines - 1.
      ENDIF.
    ENDIF.
  ENDLOOP.

ENDFORM.                              " FCODE_DELETE_ROW

*&---------------------------------------------------------------------*
*&      Form  COMPUTE_SCROLLING_IN_TC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_TC_NAME  name of tablecontrol
*      -->P_OK       ok code
*----------------------------------------------------------------------*
FORM compute_scrolling_in_tc USING    p_tc_name
                                      p_ok.
*&SPWIZARD: BEGIN OF LOCAL DATA----------------------------------------*
  DATA l_tc_new_top_line     TYPE i.
  DATA l_tc_name             LIKE feld-name.
  DATA l_tc_lines_name       LIKE feld-name.
  DATA l_tc_field_name       LIKE feld-name.

  FIELD-SYMBOLS <tc>         TYPE cxtab_control.
  FIELD-SYMBOLS <lines>      TYPE i.
*&SPWIZARD: END OF LOCAL DATA------------------------------------------*

  ASSIGN (p_tc_name) TO <tc>.
*&SPWIZARD: get looplines of TableControl                              *
  CONCATENATE 'G_' p_tc_name '_LINES' INTO l_tc_lines_name.
  ASSIGN (l_tc_lines_name) TO <lines>.


*&SPWIZARD: is no line filled?                                         *
  IF <tc>-lines = 0.
*&SPWIZARD: yes, ...                                                   *
    l_tc_new_top_line = 1.
  ELSE.
*&SPWIZARD: no, ...                                                    *
    CALL FUNCTION 'SCROLLING_IN_TABLE'
         EXPORTING
              entry_act             = <tc>-top_line
              entry_from            = 1
              entry_to              = <tc>-lines
              last_page_full        = 'X'
              loops                 = <lines>
              ok_code               = p_ok
              overlapping           = 'X'
         IMPORTING
              entry_new             = l_tc_new_top_line
         EXCEPTIONS
*              NO_ENTRY_OR_PAGE_ACT  = 01
*              NO_ENTRY_TO           = 02
*              NO_OK_CODE_OR_PAGE_GO = 03
              OTHERS                = 0.
  ENDIF.

*&SPWIZARD: get actual tc and column                                   *
  GET CURSOR FIELD l_tc_field_name
             AREA  l_tc_name.

  IF syst-subrc = 0.
    IF l_tc_name = p_tc_name.
*&SPWIZARD: et actual column                                           *
      SET CURSOR FIELD l_tc_field_name LINE 1.
    ENDIF.
  ENDIF.

*&SPWIZARD: set the new top line                                       *
  <tc>-top_line = l_tc_new_top_line.


ENDFORM.                              " COMPUTE_SCROLLING_IN_TC

*&---------------------------------------------------------------------*
*&      Form  FCODE_TC_MARK_LINES
*&---------------------------------------------------------------------*
*       marks all TableControl lines
*----------------------------------------------------------------------*
*      -->P_TC_NAME  name of tablecontrol
*----------------------------------------------------------------------*
FORM fcode_tc_mark_lines USING p_tc_name
                               p_table_name
                               p_mark_name.
*&SPWIZARD: EGIN OF LOCAL DATA-----------------------------------------*
  DATA l_table_name       LIKE feld-name.

  FIELD-SYMBOLS <tc>         TYPE cxtab_control.
  FIELD-SYMBOLS <table>      TYPE STANDARD TABLE.
  FIELD-SYMBOLS <wa>.
  FIELD-SYMBOLS <mark_field>.
*&SPWIZARD: END OF LOCAL DATA------------------------------------------*

  ASSIGN (p_tc_name) TO <tc>.

*&SPWIZARD: get the table, which belongs to the tc                     *
  CONCATENATE p_table_name '[]' INTO l_table_name. "table body
  ASSIGN (l_table_name) TO <table>.                "not headerline

*&SPWIZARD: mark all filled lines                                      *
  LOOP AT <table> ASSIGNING <wa>.

*&SPWIZARD: access to the component 'FLAG' of the table header         *
    ASSIGN COMPONENT p_mark_name OF STRUCTURE <wa> TO <mark_field>.

    <mark_field> = 'X'.
  ENDLOOP.
ENDFORM.                                          "fcode_tc_mark_lines

*&---------------------------------------------------------------------*
*&      Form  FCODE_TC_DEMARK_LINES
*&---------------------------------------------------------------------*
*       demarks all TableControl lines
*----------------------------------------------------------------------*
*      -->P_TC_NAME  name of tablecontrol
*----------------------------------------------------------------------*
FORM fcode_tc_demark_lines USING p_tc_name
                                 p_table_name
                                 p_mark_name .
*&SPWIZARD: BEGIN OF LOCAL DATA----------------------------------------*
  DATA l_table_name       LIKE feld-name.

  FIELD-SYMBOLS <tc>         TYPE cxtab_control.
  FIELD-SYMBOLS <table>      TYPE STANDARD TABLE.
  FIELD-SYMBOLS <wa>.
  FIELD-SYMBOLS <mark_field>.
*&SPWIZARD: END OF LOCAL DATA------------------------------------------*

  ASSIGN (p_tc_name) TO <tc>.

*&SPWIZARD: get the table, which belongs to the tc                     *
  CONCATENATE p_table_name '[]' INTO l_table_name. "table body
  ASSIGN (l_table_name) TO <table>.                "not headerline

*&SPWIZARD: demark all filled lines                                    *
  LOOP AT <table> ASSIGNING <wa>.

*&SPWIZARD: access to the component 'FLAG' of the table header         *
    ASSIGN COMPONENT p_mark_name OF STRUCTURE <wa> TO <mark_field>.

    <mark_field> = space.
  ENDLOOP.
ENDFORM.                                          "fcode_tc_mark_lines

*&SPWIZARD: DECLARATION OF TABLECONTROL 'TABVINC' ITSELF
CONTROLS: tabvinc TYPE TABLEVIEW USING SCREEN 0001.

*&SPWIZARD: LINES OF TABLECONTROL 'TABVINC'
DATA:     g_tabvinc_lines  LIKE sy-loopc.

*&SPWIZARD: OUTPUT MODULE FOR TC 'TABVINC'. DO NOT CHANGE THIS LINE!
*&SPWIZARD: UPDATE LINES FOR EQUIVALENT SCROLLBAR
MODULE tabvinc_change_tc_attr OUTPUT.
  DESCRIBE TABLE it_vinc LINES tabvinc-lines.
ENDMODULE.                    "TABVINC_CHANGE_TC_ATTR OUTPUT

*&SPWIZARD: INPUT MODUL FOR TC 'TABVINC'. DO NOT CHANGE THIS LINE!
*&SPWIZARD: MARK TABLE
MODULE tabvinc_mark INPUT.
  DATA: g_tabvinc_wa2 LIKE LINE OF it_vinc.
  IF tabvinc-line_sel_mode = 1
  AND zdco_nota_vinc-mark = 'X'.
    LOOP AT it_vinc INTO g_tabvinc_wa2
      WHERE mark = 'X'.
      g_tabvinc_wa2-mark = ''.
      MODIFY it_vinc
        FROM g_tabvinc_wa2
        TRANSPORTING mark.
    ENDLOOP.
  ENDIF.
  MODIFY it_vinc
    FROM zdco_nota_vinc
    INDEX tabvinc-current_line
    TRANSPORTING mark.
ENDMODULE.                    "TABVINC_MARK INPUT

*&SPWIZARD: INPUT MODULE FOR TC 'TABVINC'. DO NOT CHANGE THIS LINE!
*&SPWIZARD: PROCESS USER COMMAND
MODULE tabvinc_user_command INPUT.
  ok_code = sy-ucomm.
  PERFORM user_ok_tc USING    'TABVINC'
                              'IT_VINC'
                              'MARK'
                     CHANGING ok_code.
  sy-ucomm = ok_code.
ENDMODULE.                    "TABVINC_USER_COMMAND INPUT


*&---------------------------------------------------------------------*
*&      Form  F_MONTA_GRAFICO
*&---------------------------------------------------------------------*
FORM f_monta_grafico .

  DATA: BEGIN OF wa_graf,
          ds_tipo_leilao  TYPE zds_tipo_leilao,
          qt_material     LIKE zdco_produtor-qt_material,
          qt_entregue     LIKE zdco_produtor-qt_entregue,
          qt_remessa      LIKE zdco_produtor-qt_remessa,
          saldo_entregue  TYPE zqt_material,
          saldo_remessa   TYPE zqt_material.
  DATA: END   OF wa_graf.

  DATA: it_graf     LIKE STANDARD TABLE OF wa_graf,
        it_graf_aux LIKE STANDARD TABLE OF wa_graf,
        wa_total    LIKE wa_graf.

  CLEAR: it_graf,
         it_graf_aux,
         wa_graf.

  SELECT zp~qt_material
         zp~qt_entregue
         zp~qt_remessa
         tl~ds_tipo_leilao
    INTO CORRESPONDING FIELDS OF TABLE it_graf_aux
    FROM zdco_produtor    AS zp
    LEFT JOIN makt AS mk ON mk~spras EQ sy-langu AND mk~matnr EQ zp~cd_material
    LEFT JOIN zdco_tipo_leilao AS tl ON tl~cd_tipo_leilao EQ zp~cd_tipo_leilao
    LEFT JOIN lfa1 AS lf ON lf~lifnr EQ zp~id_fornecedor
    LEFT JOIN t001w AS t0 ON t0~werks EQ zp~cd_centro
    LEFT JOIN t001l AS t1 ON t1~werks EQ zp~cd_centro AND t1~lgort EQ zp~cd_safra
   WHERE zp~nu_dco IN p_nu_dco
     AND zp~nr_dco IN p_nr_dco
     AND zp~dt_lancamento IN p_dt_lct
     AND zp~id_fornecedor IN p_id_for
     AND zp~cd_material IN p_cd_mat
     AND zp~cd_centro IN p_cd_ctr
     AND zp~cd_safra IN p_cd_sf
     AND zp~cd_tipo_leilao IN p_tp_lei
     AND zp~vbeln IN p_vbeln.

  SORT it_graf_aux BY ds_tipo_leilao.

  LOOP AT it_graf_aux INTO wa_graf.
    AT NEW ds_tipo_leilao.
      CLEAR wa_total.
      wa_total-ds_tipo_leilao = wa_graf-ds_tipo_leilao.
    ENDAT.
    wa_total-qt_material = wa_total-qt_material + wa_graf-qt_material.
    wa_total-qt_entregue = wa_total-qt_entregue + wa_graf-qt_entregue.
    wa_total-qt_remessa  = wa_total-qt_remessa  + wa_graf-qt_remessa.
    AT END OF ds_tipo_leilao.
      wa_total-saldo_entregue = wa_total-qt_material - wa_total-qt_entregue.
      wa_total-saldo_remessa  = wa_total-qt_material - wa_total-qt_remessa .
      APPEND wa_total TO it_graf.
    ENDAT.
  ENDLOOP.

  SORT it_graf BY ds_tipo_leilao.

  DATA: BEGIN OF itab_data OCCURS 0,
  dataname(15),
  quantity1 TYPE i,
  quantity2 TYPE i,
  quantity3 TYPE i,
  quantity4 TYPE i,
  quantity5 TYPE i,
  END OF itab_data.

  DATA: BEGIN OF itab_options OCCURS 0,
  option(20),
  END OF itab_options.

  LOOP AT it_graf INTO wa_graf.
    itab_data-dataname  = wa_graf-ds_tipo_leilao.
    itab_data-quantity1 = wa_graf-qt_material.
    itab_data-quantity2 = wa_graf-qt_entregue.
    itab_data-quantity3 = wa_graf-qt_remessa.
    itab_data-quantity4 = wa_graf-saldo_entregue.
    itab_data-quantity5 = wa_graf-saldo_remessa.
  ENDLOOP.

  CALL FUNCTION 'GRAPH_MATRIX_3D'
    EXPORTING
      col1   = 'Qtd. Material'
      col2   = 'Qtd. Entregue'
      col3   = 'Qtd. Remessa'
      col4   = 'Sld. Entregar'
      col5   = 'Sld. Remessa'
      titl   = 'Demonstrativo de Quantidades/Saldos'
    TABLES
      data   = it_graf
      opts   = itab_options
    EXCEPTIONS
      OTHERS = 1.

ENDFORM.                    " F_MONTA_GRAFICO
