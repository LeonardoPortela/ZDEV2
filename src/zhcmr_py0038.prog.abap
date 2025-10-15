*&---------------------------------------------------------------------*
*& Report  ZHCMR_PY0038
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT zhcmr_py0038.

SELECTION-SCREEN BEGIN OF BLOCK 1.
  PARAMETERS: p_chave TYPE char30 NO-DISPLAY. "Parâmetro que é preenchido quando chamado pelo report ZHCM_HRST_PA_COCKPIT
SELECTION-SCREEN END OF BLOCK 1.


DATA: t_saida TYPE TABLE OF zhcms_return_periodos.

DATA: v_formname TYPE tdsfname VALUE 'ZHCMS_PY0002',
      v_name     TYPE rs38l_fnam.

DATA: e_autentica       TYPE string,
      e_cd_autenticacao TYPE zde_id_autenticacao,
      ds_autenticacao   TYPE dbcon_pwd.

DATA: t_saida_hole  TYPE TABLE OF zhcms_ret_holerite,
      w_saida_hole  TYPE zhcms_ret_holerite,
      t_objid_hole  TYPE TABLE OF zhcms_objid,
      t_event_hole  TYPE TABLE OF zhcms_ret_holerite_eventos,
      w_saida_total TYPE zhcms_ret_holerite_totais.

PARAMETERS: ppernr LIKE p0002-pernr OBLIGATORY.

SELECTION-SCREEN BEGIN OF BLOCK tp000 WITH FRAME TITLE TEXT-001.
  PARAMETERS: panopr TYPE zed_anopr OBLIGATORY.
  PARAMETERS: pmespr TYPE zed_mespr OBLIGATORY.
SELECTION-SCREEN END OF BLOCK tp000 .

SELECTION-SCREEN BEGIN OF BLOCK tp001 WITH FRAME TITLE TEXT-002.
  PARAMETERS: ptfnor RADIOBUTTON GROUP rad1 DEFAULT 'X',
              ptf13  RADIOBUTTON GROUP rad1,
              ptfprd RADIOBUTTON GROUP rad1.
SELECTION-SCREEN END OF BLOCK tp001 .

PARAMETERS: pxstring TYPE char01 NO-DISPLAY,
            pnoauten TYPE char01 NO-DISPLAY,
            pcodauti TYPE zde_id_autenticacao NO-DISPLAY,
            pautenti TYPE dbcon_pwd NO-DISPLAY,
            ptimestp TYPE char25 NO-DISPLAY.

START-OF-SELECTION.

  DATA: lc_txt_proventos TYPE c LENGTH 30.
  DATA: lc_txt_descontos TYPE c LENGTH 30.

  SELECT SINGLE * INTO @DATA(wa_zhcmt0004)
    FROM zhcmt0004
   WHERE anopr EQ @panopr
     AND mespr EQ @pmespr.

  IF sy-subrc IS NOT INITIAL.
    MESSAGE 'Ano/Mês não apurado!' TYPE 'S' DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  CALL FUNCTION 'ZHCMF_RET_LABORE_HOLERITE'
    EXPORTING
      pernr   = ppernr
      begda   = wa_zhcmt0004-inifp
      endda   = wa_zhcmt0004-fimfp
    TABLES
      t_saida = t_saida_hole
      t_objid = t_objid_hole.

  DATA: i_ocrsn TYPE pc261-ocrsn.

  CASE abap_true.
    WHEN ptfnor.
      i_ocrsn = space.
      DELETE t_saida_hole WHERE ocrsn NE space.
    WHEN ptf13.
      IF pmespr EQ 11.
        i_ocrsn = '131P'.
        DELETE t_saida_hole WHERE ocrsn NE '131P'.
      ELSE.
        i_ocrsn = '1313'.
        DELETE t_saida_hole WHERE ocrsn NE '1313'.
      ENDIF.
    WHEN ptfprd.
      i_ocrsn = 'PPRP'.
      DELETE t_saida_hole WHERE ocrsn NE 'PPRP'.
  ENDCASE.

  DESCRIBE TABLE t_saida_hole LINES DATA(qtd_linhas).

  IF qtd_linhas IS INITIAL.
    MESSAGE 'Sem holerite para este Funcionário!' TYPE 'S' DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  READ TABLE t_saida_hole INDEX 1 INTO w_saida_hole.

  CALL FUNCTION 'ZHCMF_RET_HOLERITE_EVENTOS'
    EXPORTING
      pernr   = ppernr
      begda   = wa_zhcmt0004-inifp
      endda   = wa_zhcmt0004-fimfp
      ocrsn   = i_ocrsn
    TABLES
      t_saida = t_event_hole.

  DELETE t_event_hole WHERE desconto EQ 0 AND provento EQ 0.

  IF t_event_hole[] IS INITIAL.
    MESSAGE 'Sem holerite para este Funcionário!' TYPE 'S' DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

*  SORT t_event_hole DESCENDING BY provento desconto.       "-- RMNI - CS1114060 - IR143313
  PERFORM sort_holerite.

  CLEAR: w_saida_total.

  LOOP AT t_event_hole INTO DATA(wa_event_hole).
    ADD wa_event_hole-provento TO w_saida_total-proventos.
    ADD wa_event_hole-desconto TO w_saida_total-descontos.
  ENDLOOP.

  w_saida_total-salario  = w_saida_hole-salariodecalculo.
  w_saida_total-liquido  = w_saida_hole-salariopago.

*  IF I_OCRSN EQ 'PPRP'.
*    W_SAIDA_TOTAL-LIQUIDO  = W_SAIDA_HOLE-SALARIOPAGO - W_SAIDA_TOTAL-DESCONTOS.
*  ENDIF.

  WRITE w_saida_total-proventos TO lc_txt_proventos.
  CONDENSE lc_txt_proventos NO-GAPS.

  WRITE w_saida_total-descontos TO lc_txt_descontos.
  CONDENSE lc_txt_descontos NO-GAPS.

  CONCATENATE lc_txt_proventos '-' lc_txt_descontos INTO w_saida_total-calc_text SEPARATED BY space.
  w_saida_total-calc_text = '(' && w_saida_total-calc_text && ')'.

  CASE abap_true.
    WHEN ptfnor.
      w_saida_total-basefgts = w_saida_hole-basefgts.
      w_saida_total-baseinss = w_saida_hole-baseinss.
      w_saida_total-baseirrf = w_saida_hole-baseirrf.
    WHEN ptf13.
      w_saida_total-basefgts = w_saida_hole-basefgts13.
      w_saida_total-baseinss = w_saida_hole-baseinss13.
      w_saida_total-baseirrf = w_saida_hole-baseirrf13.
    WHEN ptfprd.
      w_saida_total-basefgts = 0.
      w_saida_total-baseinss = 0.
      w_saida_total-baseirrf = w_saida_hole-baseirrfpart.
  ENDCASE.

  w_saida_total-valor_fgts = w_saida_hole-vlr_fgts.


  DATA: lc_control_parameters TYPE ssfctrlop,
        lc_output_options     TYPE ssfcompop,
        st_job_output_info    TYPE ssfcrescl,
        bin_filesize          TYPE i,
        ls_pdf_string_x       TYPE xstring,
        pdf_tab               LIKE tline OCCURS 0 WITH HEADER LINE,
        lt_pdf                TYPE TABLE OF char80,
        root                  TYPE REF TO zcl_memory_variaveis,
        oref                  TYPE REF TO zcl_memory_variaveis.

  IF pxstring EQ abap_true.

    IF ptimestp IS NOT INITIAL.
      CONCATENATE 'PDF_Folha' ppernr panopr pmespr ptimestp INTO DATA(nm_instance).
    ELSE.
      CONCATENATE 'PDF_Folha' ppernr panopr pmespr INTO nm_instance.
    ENDIF.

    DO 5 TIMES.
      WAIT UP TO 2 SECONDS.
      TRY.
          DATA(handle) = zcl_memory_variaveis_area=>attach_for_read( inst_name = CONV #( nm_instance ) ).
          DATA(ck_instance) = abap_true.
          handle->detach( ).
          EXIT.
        CATCH cx_shm_attach_error INTO DATA(lva_attach_error).
          ck_instance = abap_false.
      ENDTRY.
    ENDDO.

    IF ck_instance EQ abap_true.
      lc_control_parameters-getotf  = 'X'.
      lc_control_parameters-no_dialog = 'X'.
      lc_control_parameters-preview = space.
      lc_control_parameters-device  = 'PRINTER'.
    ENDIF.
  ENDIF.

  CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
    EXPORTING
      formname           = v_formname
    IMPORTING
      fm_name            = v_name
    EXCEPTIONS
      no_form            = 1
      no_function_module = 2
      OTHERS             = 3.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  IF pnoauten EQ abap_false.

    """"""""""""""""""""""""""""""
    CALL FUNCTION 'ZSMARTFORMS_AUTENTICA'
      EXPORTING
        i_smartform       = v_formname
      IMPORTING
        e_cd_autenticacao = e_cd_autenticacao
        e_autentica       = e_autentica.

    ds_autenticacao = e_autentica.
  ELSE.
    ds_autenticacao   = pautenti.
    e_cd_autenticacao = pcodauti.
  ENDIF.
  """"""""""""""""""""""""""""""""

  IF ds_autenticacao IS INITIAL.
    MESSAGE 'Sem código de autenticação informada!' TYPE 'S' DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  lc_output_options-tddelete = abap_true.

  CALL FUNCTION v_name
    EXPORTING
      control_parameters = lc_control_parameters
      output_options     = lc_output_options
      wa_dados           = w_saida_hole
      wa_dados_total     = w_saida_total
      ds_autenticacao    = ds_autenticacao
    IMPORTING
      job_output_info    = st_job_output_info
    TABLES
      t_event_hole       = t_event_hole
    EXCEPTIONS
      formatting_error   = 1
      internal_error     = 2
      send_error         = 3
      user_canceled      = 4
      OTHERS             = 5.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  IF pxstring EQ abap_true.

    CHECK ( st_job_output_info-otfdata[] IS NOT INITIAL ).

    CALL FUNCTION 'CONVERT_OTF'
      EXPORTING
        format        = 'PDF'
        max_linewidth = 132
      IMPORTING
        bin_filesize  = bin_filesize
        bin_file      = ls_pdf_string_x
      TABLES
        otf           = st_job_output_info-otfdata[]
        lines         = pdf_tab.

    CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
      EXPORTING
        buffer     = ls_pdf_string_x
      TABLES
        binary_tab = lt_pdf.

    DO 5 TIMES.
      WAIT UP TO 2 SECONDS.
      TRY.
          handle = zcl_memory_variaveis_area=>attach_for_write( inst_name = CONV #( nm_instance ) ).
          CREATE OBJECT root AREA HANDLE handle.
          handle->set_root( root ).
          CREATE OBJECT root AREA HANDLE handle TYPE zcl_memory_variaveis.
          oref ?= root.
          oref->set_texto_xstring( i_xstring = ls_pdf_string_x
             )->set_texto_otf( i_otf = st_job_output_info-otfdata
             )->set_autenticacao( i_ds_autenticacao = ds_autenticacao
             )->set_id_autenticacao( i_cd_autenticacao = e_cd_autenticacao
             ).
          CLEAR oref.
          handle->set_root( root ).
          handle->detach_commit( ).
          LEAVE PROGRAM.
          EXIT.
        CATCH cx_shm_attach_error INTO lva_attach_error.
      ENDTRY.
    ENDDO.

  ELSE.

    lc_control_parameters-getotf  = 'X'.
    lc_control_parameters-no_dialog = 'X'.
    lc_control_parameters-preview = space.
    lc_control_parameters-device  = 'PRINTER'.
    lc_output_options-tddelete = abap_true.

    CALL FUNCTION v_name
      EXPORTING
        control_parameters = lc_control_parameters
        output_options     = lc_output_options
        wa_dados           = w_saida_hole
        wa_dados_total     = w_saida_total
        ds_autenticacao    = ds_autenticacao
      IMPORTING
        job_output_info    = st_job_output_info
      TABLES
        t_event_hole       = t_event_hole
      EXCEPTIONS
        formatting_error   = 1
        internal_error     = 2
        send_error         = 3
        user_canceled      = 4
        OTHERS             = 5.

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 DISPLAY LIKE 'E'.
      EXIT.
    ENDIF.

    CHECK ( st_job_output_info-otfdata[] IS NOT INITIAL ).

    CALL FUNCTION 'CONVERT_OTF'
      EXPORTING
        format        = 'PDF'
        max_linewidth = 132
      IMPORTING
        bin_filesize  = bin_filesize
        bin_file      = ls_pdf_string_x
      TABLES
        otf           = st_job_output_info-otfdata[]
        lines         = pdf_tab.

    CALL FUNCTION 'ZSMARTFORMS_SET_XSTRING'
      EXPORTING
        i_cd_autenticacao = e_cd_autenticacao
        i_xstring         = ls_pdf_string_x.
  ENDIF.
*&---------------------------------------------------------------------*
*&      Form  SORT_HOLERITE
*&---------------------------------------------------------------------*
*       Organiza as rubricas no holerite pelo código e pelo tipo.
*----------------------------------------------------------------------*
FORM sort_holerite.
  DATA:
    wl_hole_ded  TYPE TABLE OF zhcms_ret_holerite_eventos,
    wl_hole_prov TYPE TABLE OF zhcms_ret_holerite_eventos.


  wl_hole_ded = t_event_hole.
  wl_hole_prov = t_event_hole.

  DELETE wl_hole_ded WHERE provento NE 0.
  SORT wl_hole_ded BY lgart.

  DELETE wl_hole_prov WHERE desconto NE 0.
  SORT wl_hole_prov BY lgart.

  t_event_hole = wl_hole_prov[].
  APPEND LINES OF wl_hole_ded TO t_event_hole.

ENDFORM.
