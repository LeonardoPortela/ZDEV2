*&---------------------------------------------------------------------*
*& Report  ZFIS32
*& Relatório - Comprovante de Pagamento
*&---------------------------------------------------------------------*
*& Developer.: Enio Jesus
*& Data......: 06.11.2015
*&---------------------------------------------------------------------*

REPORT  zfis32.
TYPE-POOLS: slis, vrm.

TABLES: zfit0091, somlreci1, bseg.

TYPES: BEGIN OF ty_saida,
         check          TYPE c,
         bukrs          TYPE zfit0091-bukrs,
         cnpj_empresa   TYPE zfit0091-cnpj_empresa,
         agencia        TYPE char10,
         conta_corrente TYPE char15,
         augbl          TYPE augbl,
         dt_pgto        TYPE sy-datum,
         vlr_pgto       TYPE zfit0091-vlr_pgto,
         lifnr          TYPE zfit0091-lifnr,
         name_lifnr     TYPE lfa1-name1,
         cnpj_cpf_forn  TYPE zfit0091-cnpj_cpf_forn,
         bco_fav        TYPE zfit0091-bco_fav,
         ag_fav         TYPE char10,
         cc_fav         TYPE char15,
         cod_autent     TYPE zfit0091-cod_autent,
         cod_barras     TYPE zcod_barras,
         recusa         TYPE zfit0091-recusado, "US - 187408 - CBRAND
         motrecusa      TYPE zfit0091-motivo,   "US - 187408 - CBRAND
       END OF ty_saida,

       BEGIN OF ty_msg_error,
         texto_breve TYPE crmt_bl_description,
       END OF ty_msg_error.

DATA: gt_zfit0091     TYPE TABLE OF zfit0091,
      gt_msg_error    TYPE TABLE OF ty_msg_error,
      gt_fcat_slis    TYPE slis_t_fieldcat_alv,
      gt_t001         TYPE TABLE OF t001,
      gt_saida        TYPE TABLE OF ty_saida,
      gt_docs         TYPE STANDARD TABLE OF docs,
      gw_tline        TYPE TABLE OF tline WITH HEADER LINE,
      gw_record       LIKE solisti1 OCCURS 0 WITH HEADER LINE,
      wl_saida        TYPE ty_saida,
      wl_zfit0091     TYPE zfit0091,
      wl_layout       TYPE slis_layout_alv,
      wl_variant      TYPE disvariant,
      wl_output_opt   TYPE ssfcompop,
      wl_document_opt TYPE ssfcrespd,
      wl_control      TYPE ssfctrlop,
      wl_job_output   TYPE ssfcrescl,
      wl_t001         TYPE t001,
      name            TYPE vrm_id,
      list            TYPE vrm_values,
      value           LIKE LINE OF list,
      v_func_name     TYPE rs38l_fnam,
      v_return        TYPE sy-subrc,
      v_empresas      TYPE char100.

*Control Parameters
DATA : it_ssfctrlop           TYPE TABLE OF ssfctrlop.
DATA : wa_ssfctrlop           TYPE ssfctrlop.

*Output Options
DATA : it_output_options      TYPE TABLE OF ssfcompop.
DATA : wa_output_options      TYPE ssfcompop.

*Job Output Info
DATA : it_job_output_info     TYPE TABLE OF ssfcrescl.
DATA : wa_job_output_info TYPE ssfcrescl,
       mi_bytecount       TYPE i,
       lv_erro(1),
       lines              LIKE tline OCCURS 100 WITH HEADER LINE.

*Job Output Options
DATA : it_job_output_options  TYPE TABLE OF ssfcresop.
DATA : wa_job_output_options  TYPE ssfcresop.

DATA: lv_devtype              TYPE rspoptype.

CONSTANTS:
  c_gerar_comprovante TYPE char4    VALUE '%GC',
  c_enviar_email      TYPE char4    VALUE '%EM',
  c_smart_forms       TYPE tdsfname VALUE 'ZFIS31'.

SELECTION-SCREEN: BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-002.
  SELECT-OPTIONS: s_bukrs  FOR zfit0091-bukrs NO INTERVALS OBLIGATORY NO-EXTENSION,
                  s_werks  FOR bseg-gsber OBLIGATORY,
                  s_lifnr  FOR zfit0091-lifnr,
                  s_dt_pg  FOR zfit0091-dt_pgto,
                  s_vlr_pg FOR zfit0091-vlr_pgto,
                  s_augbl  FOR zfit0091-augbl.
  SELECTION-SCREEN SKIP 1.

  PARAMETERS p_banco AS LISTBOX VISIBLE LENGTH 22 DEFAULT '1' OBLIGATORY.

  SELECTION-SCREEN SKIP 1.
  SELECTION-SCREEN BEGIN OF LINE.
    PARAMETERS: p_comp RADIOBUTTON GROUP g1.
    SELECTION-SCREEN COMMENT 2(50) TEXT-009 FOR FIELD p_comp.
  SELECTION-SCREEN END OF LINE.

  SELECTION-SCREEN BEGIN OF LINE.
    PARAMETERS: p_rec RADIOBUTTON GROUP g1.
    SELECTION-SCREEN COMMENT 2(50) TEXT-010 FOR FIELD p_rec.
  SELECTION-SCREEN END OF LINE.

  SELECTION-SCREEN BEGIN OF LINE.
    PARAMETERS: p_todo RADIOBUTTON GROUP g1.
    SELECTION-SCREEN COMMENT 2(20) TEXT-011 FOR FIELD p_todo.

  SELECTION-SCREEN END OF LINE.


SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF SCREEN 0110 AS SUBSCREEN.
  SELECT-OPTIONS: s_email FOR somlreci1-receiver NO INTERVALS.

SELECTION-SCREEN END OF SCREEN 0110.

AT SELECTION-SCREEN OUTPUT.
  name       = 'P_BANCO'.

  value-key  = '1'.
  value-text = '001 - BANCO DO BRASIL'.
  APPEND value TO list.

  value-key  = '2'.
  value-text = '237 - BANCO BRADESCO'.
  APPEND value TO list.

  value-key  = '3'.
  value-text = '399 - BANCO HSBC'.
  APPEND value TO list.

  value-key  = '4'.
  value-text = '341 - BANCO ITAÚ'.
  APPEND value TO list.

  value-key  = '5'.
  value-text = '748 - SICREDI'.
  APPEND value TO list.

  value-key  = '6'.
  value-text = '422 - SAFRA'.
  APPEND value TO list.



  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      id              = name
      values          = list
    EXCEPTIONS
      id_illegal_name = 1
      OTHERS          = 2.

START-OF-SELECTION.

  "// Autorização empresa
  PERFORM check_authorization USING
                              'ZFI_FINNET'
                              s_bukrs-low
                              'BUKRS'
                              CHANGING
                              v_return.

  IF ( v_return IS NOT INITIAL ).
    MESSAGE s836(sd) WITH TEXT-006 s_bukrs-low '.' DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  "// Autorização centro
  LOOP AT s_werks.
    PERFORM check_authorization USING
                                'ZFI_FINNET'
                                s_werks-low
                                'GSBER'
                                CHANGING
                                v_return.

    IF ( v_return IS NOT INITIAL ).
      MESSAGE s836(sd) WITH TEXT-007 s_werks-low '.' DISPLAY LIKE 'E'.
      EXIT.
    ENDIF.
  ENDLOOP.

  CHECK ( v_return IS INITIAL ).
  PERFORM seleciona_dados.

END-OF-SELECTION.

FORM check_authorization USING
                         p_object
                         p_field
                         p_id
                         CHANGING
                         return.

  AUTHORITY-CHECK OBJECT p_object ID p_id
  FIELD p_field.

  return = sy-subrc.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  SELECIONA_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM seleciona_dados.
  DATA lw_lfa1_aux TYPE lfa1.
  DATA lw_bseg     TYPE bseg.
  DATA doc_estorno TYPE bkpf-stblg.

  READ TABLE list INTO value WITH KEY key = p_banco.

*** US - 187408 - Inicio - CBRAND
  IF p_rec = 'X'.
    SELECT *
      FROM zfit0091
    INTO TABLE gt_zfit0091
     WHERE bukrs    IN s_bukrs
       AND lifnr    IN s_lifnr
       AND dt_pgto  IN s_dt_pg
       AND vlr_pgto IN s_vlr_pg
       AND augbl    IN s_augbl
       AND banco    EQ value-text(3)
       AND recusado EQ p_rec.
  ELSEIF p_comp = 'X'.
    SELECT *
      FROM zfit0091
    INTO TABLE gt_zfit0091
     WHERE bukrs    IN s_bukrs
       AND lifnr    IN s_lifnr
       AND dt_pgto  IN s_dt_pg
       AND vlr_pgto IN s_vlr_pg
       AND augbl    IN s_augbl
       AND banco    EQ value-text(3)
       AND recusado NE p_comp.
  ELSE.
*** US - 187408 - Fim - CBRAND
    SELECT *
      FROM zfit0091
    INTO TABLE gt_zfit0091
      WHERE bukrs    IN s_bukrs
        AND lifnr    IN s_lifnr
        AND dt_pgto  IN s_dt_pg
        AND vlr_pgto IN s_vlr_pg
        AND augbl    IN s_augbl
        AND banco    EQ value-text(3).
  ENDIF.

  LOOP AT gt_zfit0091 INTO wl_zfit0091.

    PERFORM f_seleciona_pagamento(zfis31)
      USING
       wl_zfit0091-bukrs
       wl_zfit0091-augbl
      CHANGING
       wl_zfit0091-dt_pgto
       lw_bseg.

    CHECK lw_bseg-gsber IN s_werks.

    PERFORM f_checar_pagamento(zfis31)
     USING
       wl_zfit0091-bukrs
       wl_zfit0091-augbl
       doc_estorno.

    IF ( doc_estorno IS INITIAL ).
      CLEAR lw_lfa1_aux.
      SELECT SINGLE *
        FROM lfa1
        INTO lw_lfa1_aux
       WHERE lifnr = wl_zfit0091-lifnr.

      PERFORM f_strlen USING wl_zfit0091-agencia
                    CHANGING wl_saida-agencia.

      PERFORM f_strlen USING wl_zfit0091-conta_corrente
                    CHANGING wl_saida-conta_corrente.

      wl_saida-bukrs         = wl_zfit0091-bukrs.
      wl_saida-cnpj_empresa  = wl_zfit0091-cnpj_empresa.
      wl_saida-augbl         = wl_zfit0091-augbl.
      wl_saida-dt_pgto       = wl_zfit0091-dt_pgto.
      wl_saida-vlr_pgto      = wl_zfit0091-vlr_pgto.
      wl_saida-lifnr         = wl_zfit0091-lifnr.
      wl_saida-name_lifnr    = lw_lfa1_aux-name1.
      IF NOT wl_zfit0091-lifnr IS INITIAL.
        wl_saida-cnpj_cpf_forn = wl_zfit0091-cnpj_cpf_forn.
      ENDIF.
      wl_saida-bco_fav       = wl_zfit0091-bco_fav.
      wl_saida-cod_autent    = wl_zfit0091-cod_autent.
      wl_saida-cod_barras    = wl_zfit0091-cod_barras.

      IF wl_zfit0091-dv_ag_fav IS INITIAL.
        wl_saida-ag_fav = wl_zfit0091-ag_fav.
      ELSE.
        CONCATENATE wl_zfit0091-ag_fav '-' wl_zfit0091-dv_ag_fav INTO wl_saida-ag_fav.
      ENDIF.

      SHIFT wl_zfit0091-cc_fav LEFT DELETING LEADING '0'.

      IF wl_zfit0091-dv_fav IS INITIAL.
        wl_saida-cc_fav = wl_zfit0091-cc_fav.
      ELSE.
        CONCATENATE wl_zfit0091-cc_fav '-' wl_zfit0091-dv_fav INTO wl_saida-cc_fav.
      ENDIF.

      wl_saida-recusa    = wl_zfit0091-recusado. "US - 187408 - CBRAND
      wl_saida-motrecusa = wl_zfit0091-motivo.   "US - 187408 - CBRAND

      APPEND wl_saida TO gt_saida.
      CLEAR wl_saida.
    ENDIF.

    CLEAR doc_estorno.
  ENDLOOP.

  IF NOT ( gt_saida IS INITIAL ).
    PERFORM f_imprime_dados.
  ELSE.
    MESSAGE 'Nenhum comprovante foi encontrado.' TYPE 'S' DISPLAY LIKE 'E'.
  ENDIF.
ENDFORM.                    "SELECIONA_DADOS
*&---------------------------------------------------------------------*
*&      Form  F_STATUS_001
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM f_status_001 USING pf_tab TYPE slis_t_extab.
  SET PF-STATUS 'STANDARD' EXCLUDING pf_tab.
ENDFORM.                    "F_STATUS_001

*&---------------------------------------------------------------------*
*&      Form  F_IMPRIME_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM f_imprime_dados.

  PERFORM alv_preenche_cat_slis USING:
  'BUKRS'          'Empresa'          '' '' ''  '',
  'AGENCIA'        'Agência'          '' '' ''  '',
  'CONTA_CORRENTE' 'Conta Corrente'   '' '' '' 'X',
  'AUGBL'          'Doc Comp'         '' '' '' 'X',
  'DT_PGTO'        'Dt Pgto'          '' '' ''  '',
  'VLR_PGTO'       'Vlr Pgto'         '' '' ''  '',
  'LIFNR'          'Fornecedor'       '' '' '' 'X',
  'NAME_LIFNR'     'Nome Fornecedor'  '30' '' ''  '',
  'CNPJ_CPF_FORN'  'CNPJ/CPF'         '' '' ''  '',
  'BCO_FAV'        'Banco'            '' '' ''  '',
  'AG_FAV'         'Agência'          '' '' ''  '',
  'CC_FAV'         'C/Corrente'       '' '' '' 'X',
  'COD_AUTENT'     'Cod Autent'       '' '' ''  '',
  'COD_BARRAS'     'Cod Barras'       '48' '' ''  '',
  'RECUSA'         'Recusa'           '06' '' ''  '',  "US - 187408 - CBRAND
  'MOTRECUSA'      'Motivo Recusa'    '100' '' ''  ''. "US - 187408 - CBRAND

  wl_variant-report       = sy-repid.
  wl_layout-box_fieldname = 'CHECK'.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program       = sy-repid
      i_callback_top_of_page   = 'TOP-OF-PAGE'
      i_callback_pf_status_set = 'F_STATUS_001'
      is_layout                = wl_layout
      is_variant               = wl_variant
      i_callback_user_command  = 'F_USER_COMMAND'
      it_fieldcat              = gt_fcat_slis
      i_save                   = 'X'
    TABLES
      t_outtab                 = gt_saida.

ENDFORM.                    "F_IMPRIME_DADOS

*&---------------------------------------------------------------------*
*&      Form  TOP-OF-PAGE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM top-of-page.
  DATA: lt_header TYPE slis_listheader,
        gt_header TYPE slis_t_listheader,
        wl_lfa1   TYPE lfa1.

  lt_header-typ  = 'H'.
  lt_header-info = TEXT-001.
  APPEND lt_header TO gt_header.

  SELECT SINGLE *
    FROM t001
    INTO wl_t001
   WHERE bukrs = s_bukrs-low.

  lt_header-typ  = 'S'.
  lt_header-key  = 'Empresa:'.

*  IF ( S_BUKRS-HIGH IS INITIAL ).
  CONCATENATE s_bukrs-low '-' wl_t001-butxt INTO lt_header-info SEPARATED BY space.
*  ELSE.
*    CONCATENATE S_BUKRS-LOW 'Até' S_BUKRS-HIGH INTO LT_HEADER-INFO SEPARATED BY SPACE.
*  ENDIF.
  APPEND lt_header TO gt_header.
  CLEAR lt_header.

  IF ( s_lifnr-low IS NOT INITIAL ).

    SELECT SINGLE *
      FROM lfa1
      INTO wl_lfa1
     WHERE lifnr = s_lifnr-low.

    lt_header-typ = 'S'.
    lt_header-key = 'Fornecedor:'.

    SHIFT s_lifnr-low  LEFT DELETING LEADING '0'.
    SHIFT s_lifnr-high LEFT DELETING LEADING '0'.

    IF ( s_lifnr-high IS INITIAL ).
      CONCATENATE s_lifnr-low '-' wl_lfa1-name1 INTO lt_header-info SEPARATED BY space.
    ELSE.
      CONCATENATE s_lifnr-high 'Até' s_lifnr-high INTO lt_header-info SEPARATED BY space.
    ENDIF.
    APPEND lt_header TO gt_header.
    CLEAR lt_header.

  ENDIF.

  IF ( s_dt_pg-low IS NOT INITIAL ).

    lt_header-typ = 'S'.
    lt_header-key = 'Dt Pagamento:'.

    DATA: date1(10), date2 TYPE char10.

    CONCATENATE s_dt_pg-low+6(2) '/' s_dt_pg-low+4(2) '/' s_dt_pg-low(4) INTO date1.
    CONCATENATE s_dt_pg-high+6(2) '/' s_dt_pg-high+4(2) '/' s_dt_pg-high(4) INTO date2.

    IF ( s_dt_pg-high IS INITIAL ).
      lt_header-info = date1.
    ELSE.
      CONCATENATE date1 'Até' date2 INTO lt_header-info SEPARATED BY space.
    ENDIF.
    APPEND lt_header TO gt_header.
    CLEAR lt_header.

  ENDIF.

  READ TABLE list INTO value WITH KEY key = p_banco.

  lt_header-typ  = 'S'.
  lt_header-key  = 'Banco:'.
  lt_header-info = value-text.

  APPEND lt_header TO gt_header.
  CLEAR lt_header.

  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      it_list_commentary = gt_header.
ENDFORM.                    "TOP-OF-PAGE

*&---------------------------------------------------------------------*
*&      Form  F_USER_COMMAND
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM f_user_command USING i_ucomm     LIKE sy-ucomm
                          i_selfield  TYPE slis_selfield.
  CASE i_ucomm.
    WHEN c_gerar_comprovante.
      REFRESH gt_zfit0091.

      LOOP AT gt_saida INTO wl_saida WHERE check = 'X'.
        SELECT *
          FROM zfit0091
     APPENDING TABLE gt_zfit0091
         WHERE bukrs = wl_saida-bukrs
           AND augbl = wl_saida-augbl
           AND recusado <> 'X'. "US - 187408 - CBRAND
      ENDLOOP.

      wl_output_opt-tddest  = 'LOCL'.
      wl_output_opt-tdimmed = 'X'.

      IF gt_zfit0091 IS NOT INITIAL. "US - 187408 - CBRAND

        PERFORM f_call_smart_forms USING wl_output_opt
                                         wl_control CHANGING wl_job_output.

      ENDIF.

    WHEN c_enviar_email.
      DATA: v_flag TYPE butxt.
      CLEAR: gt_zfit0091, v_empresas, s_email[].

      LOOP AT gt_saida INTO wl_saida WHERE check = 'X'.

        SELECT *
          FROM zfit0091
     APPENDING TABLE gt_zfit0091
         WHERE bukrs = wl_saida-bukrs
           AND augbl = wl_saida-augbl.
      ENDLOOP.

      IF sy-subrc IS INITIAL.
        CALL SCREEN 0200 STARTING AT 8 5
                           ENDING AT 70 6.
      ELSE.
        MESSAGE 'Nenhum comprovante selecionado.' TYPE 'S' DISPLAY LIKE 'E'.
      ENDIF.
    WHEN OTHERS.
  ENDCASE.
ENDFORM.                    "F_USER_COMMAND

*&---------------------------------------------------------------------*
*&      Form  F_CALL_SMART_FORMS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM f_call_smart_forms USING i_output_options TYPE ssfcompop
                              i_control_params TYPE ssfctrlop
                        CHANGING
                              c_job_output     TYPE ssfcrescl.

  CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
    EXPORTING
      formname = c_smart_forms
    IMPORTING
      fm_name  = v_func_name.

  CLEAR wl_job_output.

  CALL FUNCTION v_func_name
    EXPORTING
      control_parameters = i_control_params
      output_options     = i_output_options
      user_settings      = ' '
    IMPORTING
      job_output_info    = c_job_output
    TABLES
      it_zfit0091        = gt_zfit0091
    EXCEPTIONS
      formatting_error   = 1
      internal_error     = 2
      send_error         = 3
      user_canceled      = 4
      OTHERS             = 5.

ENDFORM.                    "F_CALL_SMART_FORMS

*&---------------------------------------------------------------------*
*&      Form  ALV_PREENCHE_CAT_SLIS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM alv_preenche_cat_slis  USING fieldname
                                  seltext_m
                                  output_leng
                                  checkbox
                                  edit
                                  no_zero.


  DATA: wl_fcat_slis TYPE slis_fieldcat_alv.

  wl_fcat_slis-fieldname   = fieldname.
  wl_fcat_slis-seltext_m   = seltext_m.
  wl_fcat_slis-checkbox    = checkbox.
  wl_fcat_slis-edit        = edit.
  wl_fcat_slis-no_zero     = no_zero.
  wl_fcat_slis-outputlen   = output_leng.

  APPEND wl_fcat_slis TO gt_fcat_slis.
  CLEAR wl_fcat_slis.
ENDFORM.                    "ALV_PREENCHE_CAT_SLIS

*&---------------------------------------------------------------------*
*&      Form  F_STRLEN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->I_TEXT     text
*----------------------------------------------------------------------*
FORM f_strlen  CHANGING i_text e_text.
  DATA: v_strlen TYPE i.

  REPLACE '-' WITH '' INTO i_text.
  v_strlen = strlen( i_text ) - 1.

  CONCATENATE i_text(v_strlen) '-' i_text+v_strlen(1)
  INTO e_text.

ENDFORM.                    "F_STRLEN

*&---------------------------------------------------------------------*
*&      Form  F_SEND_EMAIL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM f_enviar_email.
  DATA: gw_objtxt   LIKE solisti1   OCCURS 0 WITH HEADER LINE,
        gw_objpack  LIKE sopcklsti1 OCCURS 0 WITH HEADER LINE,
        gw_reclist  LIKE somlreci1  OCCURS 0 WITH HEADER LINE,
        gw_objbin   LIKE solisti1   OCCURS 0 WITH HEADER LINE,

        wl_doc_chng TYPE sodocchgi1,
        wl_objhead  TYPE soli_tab,
        v_lines_txt TYPE i,
        v_lines_bin TYPE i.

  wl_control-no_dialog   = 'X'.
  wl_control-preview     = space.
  wl_control-device      = 'PRINTER'.
  wl_control-getotf      = 'X'.

  wl_output_opt-tddest   = 'LOCL'.
  wl_output_opt-tdimmed  = 'X'.
  wl_output_opt-tdnewid  = 'X'.
  wl_output_opt-tdnoarch = 'X'.

  PERFORM f_call_smart_forms USING wl_output_opt
                                   wl_control CHANGING wl_job_output.

*  PERFORM f_get_smart_forms USING wl_output_opt
*                                   wl_control CHANGING wl_job_output.

  PERFORM f_generate_pdf USING wl_job_output.

  REFRESH: gw_objtxt,
           gw_objpack.

  gw_objbin[] = gw_record[].

* ______Corpo do E-mail______

  gw_objtxt = TEXT-005. APPEND gw_objtxt.
  gw_objtxt = space.    APPEND gw_objtxt.
  gw_objtxt = TEXT-003. APPEND gw_objtxt.
  gw_objtxt = TEXT-004. APPEND gw_objtxt.
* ___________________________

  DESCRIBE TABLE gw_objtxt LINES v_lines_txt.
  READ TABLE gw_objtxt INDEX v_lines_txt.

  SELECT SINGLE *
    FROM t001
    INTO wl_t001
   WHERE bukrs = s_bukrs-low.

  CONCATENATE 'Comprovante de pagamento' wl_t001-butxt INTO wl_doc_chng-obj_descr SEPARATED BY space.
  wl_doc_chng-obj_name   = 'smartforms'.
  wl_doc_chng-sensitivty = 'F'.
  wl_doc_chng-doc_size   = v_lines_txt * 255.

* Main Text
  gw_objpack-head_start = 1.
  gw_objpack-head_num   = 0.
  gw_objpack-body_start = 1.
  gw_objpack-body_num   = v_lines_txt.
  gw_objpack-doc_type   = 'RAW'.
  APPEND gw_objpack.

* Attachment (pdf-Attachment)
  gw_objpack-transf_bin = 'X'.
  gw_objpack-head_start = 1.
  gw_objpack-head_num   = 0.
  gw_objpack-body_start = 1.

  DESCRIBE TABLE gw_objbin LINES v_lines_bin.
  READ TABLE gw_objbin INDEX v_lines_bin.

  gw_objpack-doc_size   = v_lines_bin * 255 .
  gw_objpack-body_num   = v_lines_bin.
  gw_objpack-doc_type   = 'PDF'.
  gw_objpack-obj_name   = 'smart'.
  gw_objpack-obj_descr  = 'Comprovante de pagamento'.
  APPEND gw_objpack.

  LOOP AT s_email.
    CLEAR gw_reclist.

    gw_reclist-receiver = s_email-low.
    gw_reclist-rec_type = 'U'.
    APPEND gw_reclist.

    CALL FUNCTION 'SO_NEW_DOCUMENT_ATT_SEND_API1'
      EXPORTING
        document_data              = wl_doc_chng
        put_in_outbox              = 'X'
        commit_work                = 'X'
      TABLES
        packing_list               = gw_objpack
        object_header              = wl_objhead
        contents_bin               = gw_objbin
        contents_txt               = gw_objtxt
        receivers                  = gw_reclist
      EXCEPTIONS
        too_many_receivers         = 1
        document_not_sent          = 2
        document_type_not_exist    = 3
        operation_no_authorization = 4
        parameter_error            = 5
        x_error                    = 6
        enqueue_error              = 7
        OTHERS                     = 8.
  ENDLOOP.
ENDFORM.                    "F_SEND_EMAIL

*&---------------------------------------------------------------------*
*&      Form  F_GENERATE_PDF
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM f_generate_pdf USING i_job_info TYPE ssfcrescl.

  DATA: gw_otf         TYPE itcoo OCCURS 0 WITH HEADER LINE,
        v_bin_filesize TYPE i,
        wl_buffer      TYPE string.

  gw_otf[] = i_job_info-otfdata[].

  CALL FUNCTION 'CONVERT_OTF'
    EXPORTING
      format                = 'PDF'
      max_linewidth         = 132
    IMPORTING
      bin_filesize          = v_bin_filesize
    TABLES
      otf                   = gw_otf
      lines                 = gw_tline
    EXCEPTIONS
      err_max_linewidth     = 1
      err_format            = 2
      err_conv_not_possible = 3
      OTHERS                = 4.

  LOOP AT gw_tline.
    TRANSLATE gw_tline USING '~'.
    CONCATENATE wl_buffer gw_tline INTO wl_buffer.
  ENDLOOP.
  TRANSLATE wl_buffer USING '~'.
  DO.
    gw_record = wl_buffer.
    APPEND gw_record.
    SHIFT wl_buffer LEFT BY 255 PLACES.
    IF ( wl_buffer IS INITIAL ).
      EXIT.
    ENDIF.
  ENDDO.
ENDFORM.                    "F_GENERATE_PDF
*&---------------------------------------------------------------------*
*&      Module  PAI_0200  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE pai_0200 INPUT.
  CASE sy-ucomm.
    WHEN 'EXIT'.
      LEAVE TO SCREEN 0.
    WHEN 'ENVIAR'.
      IF s_email IS INITIAL.
        MESSAGE 'Informar um e-mail para o destinatário.' TYPE 'S' DISPLAY LIKE 'E'.
      ELSE.
        PERFORM f_enviar_email.

        MESSAGE 'Comprovante enviado com sucesso!' TYPE 'I' DISPLAY LIKE 'S'.
        LEAVE TO SCREEN 0.
      ENDIF.
  ENDCASE.
ENDMODULE.                 " PAI_0200  INPUT
*&---------------------------------------------------------------------*
*&      Module  STATUS_0200  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0200 OUTPUT.
  SET PF-STATUS '0200'.
  SET TITLEBAR '0200'.

ENDMODULE.                 " STATUS_0200  OUTPUT
*&---------------------------------------------------------------------*
*& Form f_get_smart_forms
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> WL_OUTPUT_OPT
*&      --> WL_CONTROL
*&      <-- WL_JOB_OUTPUT
*&---------------------------------------------------------------------*
FORM f_get_smart_forms USING i_output_options TYPE ssfcompop
                              i_control_params TYPE ssfctrlop
                        CHANGING
                              c_job_output     TYPE ssfcrescl.


*  CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
*    EXPORTING
*      formname = c_smart_forms
*    IMPORTING
*      fm_name  = v_func_name.
*
*  CLEAR wl_job_output.
*
*
**Parâmetros do smartform para geração do PDF
*wa_ssfctrlop-NO_DIALOG = abap_true.
*wa_ssfctrlop-getotf    = abap_true.
*APPEND wa_ssfctrlop TO it_ssfctrlop.
*
*CALL FUNCTION 'SSF_GET_DEVICE_TYPE'
*EXPORTING
*i_language    = SY-LANGU
*i_application = 'SAPDEFAULT'
*IMPORTING
*e_devtype     = lv_devtype.
*
*
*wa_output_options-tdprinter = lv_devtype.
*wa_output_options-tdnoprev  = abap_true.
*wa_output_options-tdimmed   = abap_true.
*wa_output_options-tdtitle   = sy-title.
*wa_output_options-tdnewid   = abap_true.
*APPEND wa_output_options TO it_output_options.
*
*
*CALL FUNCTION v_func_name
*EXPORTING
*control_parameters = wa_ssfctrlop
*output_options     = wa_output_options
*IMPORTING
*job_output_info    = c_job_output
*job_output_options = wa_job_output_options
*TABLES
*it_zfit0091        = gt_zfit0091
*EXCEPTIONS
*formatting_error   = 1
*internal_error     = 2
*send_error         = 3
*user_canceled      = 4
*OTHERS             = 5.

ENDFORM.
