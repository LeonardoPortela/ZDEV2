*&---------------------------------------------------------------------*
*&  Include           ZAA01F01
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  F_BUSCA_DADOS_TELA
*&---------------------------------------------------------------------*
*       text

FORM f_busca_dados_tela .

  CLEAR w_buzei.
  REFRESH t_hip.
  SHIFT w_anln2 RIGHT DELETING TRAILING space.
  TRANSLATE w_anln2 USING ' 0'.

  SELECT SINGLE txt50 txa50" ANLHTXT
    FROM anla INTO (w_txt50, w_txa50)
    WHERE bukrs = w_bukrs AND
          anln1 = w_anln1 AND
          anln2 = w_anln2.
  IF sy-subrc <> 0.
    MESSAGE i001(aa) WITH w_anln1 w_anln2 w_bukrs.
    LEAVE SCREEN.
  ELSE.

    SELECT SINGLE anlhtxt FROM anlh
      INTO w_anlhtxt
      WHERE bukrs = w_bukrs AND
            anln1 = w_anln1.

    SELECT SINGLE * FROM zaa_controle_doc
      INTO zaa_controle_doc
      WHERE bukrs = w_bukrs AND
            anln1 = w_anln1 AND
            anln2 = w_anln2.
    IF sy-subrc <> 0 AND
       w_ucomm  <> 'CRIA'.
      MESSAGE i000(z01) WITH 'Imobilizado não cadastro para Dados de '
                             'Imoveis/Hipotecas'.
      LEAVE SCREEN.
    ENDIF.

    SELECT * FROM zaa_controle_hip
      INTO TABLE t_hip
      WHERE bukrs = w_bukrs AND
            anln1 = w_anln1 and
            anln2 = w_anln2.
    IF sy-subrc = 0.
      IF w_ucomm = 'CRIA'.
        MESSAGE i002(aa) WITH w_anln1 w_anln2.
        LEAVE SCREEN.
      ENDIF.
      DESCRIBE TABLE t_hip LINES w_buzei.
    ENDIF.
  ENDIF.

  PERFORM f_busca_descricoes USING zaa_controle_doc-pais
                                   zaa_controle_doc-estado
                                   zaa_controle_doc-estado_com
                                   zaa_controle_doc-feins.

  htheader-tdobject = 'ZAA01'.
*  HTHEADER-TDNAME = 450000000000010
  CONCATENATE w_anln1 w_anln2 INTO htheader-tdname.
  htheader-tdid = 'ZAA1'.
  htheader-tdspras = sy-langu.
  htheader-tdform = 'SYSTEM'.
  htheader-tdversion = '0'.
  htheader-tdfuser = sy-uname.
  htheader-tdfreles = sy-saprl.
  htheader-tdfdate = sy-datum.
  htheader-tdftime = sy-uzeit.
  htheader-tdldate = '0'.
  htheader-tdltime = '0'.
  htheader-tdlinesize = '70'.
  htheader-tdtxtlines = '0'.
  htheader-tdtranstat = '0'.

  CLEAR ct_docu.
  REFRESH ct_docu.

  CALL FUNCTION 'READ_TEXT'
    EXPORTING
*   CLIENT                        = SY-MANDT
      id                            = htheader-tdid
      language                      = sy-langu
      name                          = htheader-tdname
      object                        = htheader-tdobject
    TABLES
      lines                         = ct_docu
   EXCEPTIONS
     id                            = 1
     language                      = 2
     name                          = 3
     not_found                     = 4
     object                        = 5
     reference_check               = 6
     wrong_access_to_archive       = 7
     OTHERS                        = 8
            .
  IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

*  IF ct_docu[] IS INITIAL.
*    w_up = ' '.
*  ELSE.
*    CLEAR w_up.
*  ENDIF.

  SELECT SINGLE gsber FROM anlz
    INTO w_gsber
    WHERE bukrs = w_bukrs AND
          anln1 = w_anln1 AND
          anln2 = w_anln2.
  IF sy-subrc = 0.
    SELECT SINGLE name FROM j_1bbranch
      INTO wt_gsber
      WHERE bukrs  = w_bukrs AND
            branch = w_gsber.

  ENDIF.

ENDFORM.                    " F_BUSCA_DADOS_TELA
*&---------------------------------------------------------------------*
*&      Form  F_SALVA_DADOS
*&---------------------------------------------------------------------*
FORM f_salva_dados .

  CLEAR w_save.

  zaa_controle_doc-bukrs = w_bukrs.
  zaa_controle_doc-anln1 = w_anln1.
  zaa_controle_doc-anln2 = w_anln2.

  INSERT zaa_controle_doc FROM zaa_controle_doc.
  IF sy-subrc <> 0.
    MODIFY zaa_controle_doc FROM zaa_controle_doc.
  ENDIF.

  LOOP AT t_hip.
    t_hip-buzei = sy-tabix.
    MODIFY t_hip.
  ENDLOOP.

  IF NOT t_hip[] IS INITIAL.
    DELETE FROM zaa_controle_hip WHERE bukrs = w_bukrs AND
                                       anln1 = w_anln1.
    MODIFY zaa_controle_hip FROM TABLE t_hip.
  ENDIF.

  COMMIT WORK AND WAIT.

  MESSAGE i000(z001) WITH 'Documento salvo!'.

  DATA: txfunction.
*txfunction = 'I'.
  IF NOT ct_docu[] IS INITIAL.
    CALL FUNCTION 'SAVE_TEXT'
      EXPORTING
        header    = htheader
      IMPORTING
        function  = txfunction
        newheader = htheader
      TABLES
        lines     = ct_docu
      EXCEPTIONS
        OTHERS    = 1.
  ENDIF.

ENDFORM.                    " F_SALVA_DADOS

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
*&---------------------------------------------------------------------*
*&      Form  F_VOLTAR
*&---------------------------------------------------------------------*
FORM f_voltar .

  DATA: program TYPE sy-repid,
        tcode   TYPE sy-tcode.

  IF w_save EQ 'X'.
    DATA: v_answer.
    CALL FUNCTION 'POPUP_TO_CONFIRM'
      EXPORTING
        titlebar              = 'Salvar'
        text_question         = 'Deseja salvar o imobilizado?'
        text_button_1         = 'Sim'
        icon_button_1         = 'ICON_OKAY'
        text_button_2         = 'Não'
        icon_button_2         = 'ICON_CANCEL'
        default_button        = '1'
        display_cancel_button = ' '
      IMPORTING
        answer                = v_answer.
    IF v_answer NE '1'.
      LEAVE TO SCREEN 100.
    ELSE.
      PERFORM f_salva_dados.
    ENDIF.
  ENDIF.
  GET PARAMETER ID 'MEN' FIELD program.
  SET PARAMETER ID 'MEN' FIELD ''.

  CLEAR: zaa_controle_doc, zaa_controle_hip.
  IF program ='ZAA02'.
    LEAVE PROGRAM.
  ELSE.
    LEAVE TO SCREEN 100.
  ENDIF.
*  ELSE.
*    SET SCREEN 0.
*    LEAVE SCREEN.
*  ENDIF.

ENDFORM.                    " F_VOLTAR
*&---------------------------------------------------------------------*
*&      Form  F_APAGA_REGISTROS
*&---------------------------------------------------------------------*
FORM f_apaga_registros .

  SELECT SINGLE * FROM zaa_controle_doc
    INTO zaa_controle_doc
    WHERE bukrs = w_bukrs AND
          anln1 = w_anln1 AND
          anln2 = w_anln2.
  IF sy-subrc <> 0.
    MESSAGE i000(z01) WITH 'Imobilizado não cadastro para Dados de '
                           'Imoveis/Hipotecas'.
    LEAVE SCREEN.
  ELSE.

    DATA: v_answer.

    CALL FUNCTION 'POPUP_TO_CONFIRM'
      EXPORTING
        titlebar              = 'Apagar'
        text_question         = 'Deseja apagar o imobilizado?'
        text_button_1         = 'Sim'
        icon_button_1         = 'ICON_OKAY'
        text_button_2         = 'Não'
        icon_button_2         = 'ICON_CANCEL'
        default_button        = '1'
        display_cancel_button = ' '
      IMPORTING
        answer                = v_answer.
    IF v_answer NE '1'.
      LEAVE SCREEN.
    ENDIF.


    DELETE FROM zaa_controle_doc WHERE bukrs = w_bukrs AND
                                       anln1 = w_anln1 AND
                                       anln2 = w_anln2.
    CLEAR zaa_controle_doc.
    IF sy-subrc = 0.
      MESSAGE i000(z01) WITH 'Registro eliminado com sucesso!'.
    ELSE.
      MESSAGE i000(z01) WITH 'Erro ao apagar o registro'.
      LEAVE SCREEN.
    ENDIF.
    DELETE FROM zaa_controle_hip WHERE bukrs = w_bukrs AND
                                       anln1 = w_anln1.

    COMMIT WORK AND WAIT.

  ENDIF.


ENDFORM.                    " F_APAGA_REGISTROS
*&---------------------------------------------------------------------*
*&      Form  F_BUSCA_DESCRICOES
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_ZAA_CONTROLE_DOC_PAIS  text
*      -->P_ZAA_CONTROLE_DOC_ESTADO  text
*      -->P_ZAA_CONTROLE_DOC_ESTADO_COM  text
*----------------------------------------------------------------------*
FORM f_busca_descricoes  USING    p_pais
                                  p_estado
                                  p_estado2
                                  p_medida.

*** Descrição do pais
  IF NOT p_pais IS INITIAL.
    SELECT SINGLE landx50 FROM t005t INTO wt_pais
      WHERE spras = sy-langu AND
            land1 = p_pais.
  ELSE.
    CLEAR wt_pais.
  ENDIF.

* Descrição do estado
  IF NOT p_estado IS INITIAL.
    SELECT SINGLE bezei FROM t005u INTO wt_estado
      WHERE spras = sy-langu AND
            land1 = p_pais   AND
            bland = p_estado.
  ELSE.
    CLEAR wt_estado.
  ENDIF.

* Descrição do estado comercial
  IF NOT p_estado2 IS INITIAL.
    SELECT SINGLE bezei FROM t005u INTO wt_estado2
      WHERE spras = sy-langu AND
            land1 = p_pais   AND
            bland = p_estado2.
  ELSE.
    CLEAR wt_estado2.
  ENDIF.

  IF NOT p_medida IS INITIAL.
    SELECT SINGLE msehl FROM t006a
      INTO wt_medida
      WHERE spras = sy-langu AND
            msehi = p_medida.
  ELSE.
    CLEAR wt_medida.
  ENDIF.
ENDFORM.                    " F_BUSCA_DESCRICOES
