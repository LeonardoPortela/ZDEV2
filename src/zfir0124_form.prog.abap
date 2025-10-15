
MODULE user_command_0100 INPUT.
  PERFORM action_process.
ENDMODULE.

MODULE user_command_0200 INPUT.
  PERFORM action_process.
ENDMODULE.

MODULE status_0100 OUTPUT.
  SET PF-STATUS 'STATUS_0100'.
  SET TITLEBAR 'T0100'.

  DATA: lo_descr  TYPE REF TO cl_abap_objectdescr,
        lo_object TYPE REF TO object.

  lo_descr ?= cl_abap_typedescr=>describe_by_name( 'LCL_REPORT_100' ).
  CREATE OBJECT lo_report_100 TYPE (lo_descr->absolute_name).

  "CREATE OBJECT lo_report_100 TYPE (lcl_report_100).

  IF p_create = abap_true.
    lo_report_100->create_data03( ).

    IF lo_report_100->it_saida03 IS INITIAL.
      MESSAGE 'Não foram encontrados dados para seleção!' TYPE 'I'.
      SET SCREEN 0.
      LEAVE SCREEN.
    ENDIF.

  ELSE.
    lo_report_100->get_data03( ).
    IF lo_report_100->it_saida03 IS INITIAL.
      MESSAGE 'Não foram encontrados dados para seleção!' TYPE 'I'.
      SET SCREEN 0.
      LEAVE SCREEN.
    ENDIF.
    lo_report_100->get_data02( ).
    lo_report_100->get_data01( ).
  ENDIF.

  lo_report_100->generate_output1( ).
  lo_report_100->generate_output2( ).
  lo_report_100->generate_output3( ).

ENDMODULE.

MODULE status_0200 OUTPUT.
  SET PF-STATUS 'STATUS_0200'.
  SET TITLEBAR 'T0200'.
  CREATE OBJECT lo_report_200.
  lo_report_200->get_data( ).
  lo_report_200->generate_output( ).
ENDMODULE.

FORM action_process.

  DATA ls_stable TYPE lvc_s_stbl.

  CASE sy-ucomm.
    WHEN 'BACK' OR 'CANCEL' OR 'EXIT'.

      IF lo_report_100 IS BOUND.
        CALL METHOD lo_report_100->o_alv01->refresh_table_display
          EXPORTING
            is_stable = ls_stable.

        CALL METHOD lo_report_100->o_alv02->refresh_table_display
          EXPORTING
            is_stable = ls_stable.

        CALL METHOD lo_report_100->o_alv03->refresh_table_display
          EXPORTING
            is_stable = ls_stable.
      ENDIF.

      IF lo_report_100 IS BOUND.
        CLEAR:         lo_report_100->o_alv01,
        lo_report_100->o_alv02,
        lo_report_100->o_alv03.
        FREE: lo_report_100->it_f4_03,
        lo_report_100->it_fieldcat_01,
        lo_report_100->it_fieldcat_02,
        lo_report_100->it_fieldcat_03,
        lo_report_100->it_saida01,
        lo_report_100->it_saida02,
        lo_report_100->it_saida03,
        lo_report_100->o_alv01,
        lo_report_100->o_alv02,
        lo_report_100->o_alv03,
        lo_report_100.
      ENDIF.

      IF lo_report_200 IS BOUND.
        CLEAR: lo_report_200->o_alv200.
        FREE: lo_report_200->it_f4,
        lo_report_200->it_fieldcat,
        lo_report_200->it_saida,
        lo_report_200->o_alv200,
        lo_report_200.
      ENDIF.

      CALL METHOD cl_gui_cfw=>flush.


      ls_stable-row = 'X'.
      ls_stable-col = 'X'.



*      SET SCREEN '0'.

      LEAVE PROGRAM.

      SET SCREEN 0.
*      LEAVE SCREEN.

    WHEN 'BT_REFRESH_100'.

      lo_report_100->set_refresh_out3( ).


    WHEN 'BT_PRINT_100'.

      PERFORM formulario.

  ENDCASE.
ENDFORM.

FORM convert_tool CHANGING p_dtini TYPE sy-datum p_dtfim TYPE sy-datum p_dtsub TYPE sy-datum p_ukurs TYPE p.

  p_dtini = |{ p_gjahr-low }{ p_monat-low }01|. " Qualquer data (formato AAAAMMDD)

  p_dtsub = |{ p_gjahr-low }{ p_monat-low }01|. " Qualquer data (formato AAAAMMDD)

  CALL FUNCTION 'SN_LAST_DAY_OF_MONTH'
    EXPORTING
      day_in       = p_dtini
    IMPORTING
      end_of_month = p_dtfim.

  CALL FUNCTION '/PICM/ADD_MONTH_TO_DATE'
    EXPORTING
      months  = 1
      olddate = p_dtini
    IMPORTING
      newdate = p_dtsub.


  DATA: lv_rate    TYPE f.

  CALL FUNCTION 'READ_EXCHANGE_RATE'
    EXPORTING
      date             = p_dtsub         " Data da taxa
      foreign_currency = 'USD'           " Moeda origem
      local_currency   = 'BRL'           " Moeda destino
      type_of_rate     = 'B'             " Tipo de cotação
    IMPORTING
      exchange_rate    = lv_rate         " Resultado da taxa
    EXCEPTIONS
      no_rate_found    = 1
      OTHERS           = 2.

  IF sy-subrc <> 0.
    p_ukurs = 0.
    MESSAGE 'Taxa de câmbio não encontrada' TYPE 'E'.
  ELSE.
    p_ukurs = ( lv_rate / 100000 ).
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form formulario
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM formulario .

*181777 - CS2022000790 - RGA
  DATA: ls_control        TYPE ssfctrlop,
        ls_options        TYPE ssfcompop,
        job_output_info   TYPE ssfcrescl,
        ls_xsfparam_line  TYPE ssfxsfp,
        v_bin_filesize    TYPE i,
        it_docs           TYPE STANDARD TABLE OF docs,
        it_lines          TYPE STANDARD TABLE OF tline,
        lv_fname          TYPE rs38l_fnam,
        lv_mail_recipient TYPE swotobjid,
        lv_mail_sender    TYPE swotobjid,
        lv_control        TYPE ssfctrlop,
        lv_name           TYPE so_name,
        lv_output         TYPE ssfcompop,
        wl_zmeng(20),
        wl_dmbtr(20),
        wl_vlrtot(20).

  DATA: i_otf       TYPE itcoo OCCURS 0 WITH HEADER LINE,
        i_tline     TYPE TABLE OF tline WITH HEADER LINE,
        i_receivers TYPE TABLE OF somlreci1 WITH HEADER LINE,
        i_record    LIKE solisti1 OCCURS 0 WITH HEADER LINE,
* Objects to send mail.
        i_objpack   LIKE sopcklsti1 OCCURS 0 WITH HEADER LINE,
        i_objtxt    LIKE solisti1 OCCURS 0 WITH HEADER LINE,
        i_objbin    LIKE solisti1 OCCURS 0 WITH HEADER LINE,
        i_reclist   LIKE somlreci1 OCCURS 0 WITH HEADER LINE,
* Work Area declarations
        wa_objhead  TYPE soli_tab,
        w_ctrlop    TYPE ssfctrlop,
        w_compop    TYPE ssfcompop,
        w_return    TYPE ssfcrescl,
        wa_doc_chng TYPE sodocchgi1,
        w_data      TYPE sodocchgi1,
        wa_buffer   TYPE string, "To convert from 132 to 255
* Variables declarations
        v_form_name TYPE rs38l_fnam,
        v_len_in    LIKE sood-objlen,
        v_len_out   LIKE sood-objlen,
        v_len_outn  TYPE i,
        v_lines_txt TYPE i,
        v_lines_bin TYPE i,
        vl_name     TYPE rs38l_fnam,
        vl_form     TYPE tdsfname.

  vl_form = 'ZFIR0006'.
*

  DATA lt_analitica TYPE ztt_analitica.
  DATA lt_sintetica TYPE ztt_sintetica.
  DATA lt_sintetica2 TYPE ztt_sintetica.

  lt_analitica =   VALUE #( FOR ls_saida01 IN lo_report_100->it_saida03
                          ( CORRESPONDING #( ls_saida01 )

                           ) ).

  lt_sintetica =   VALUE #( FOR ls_saida02 IN lo_report_100->it_saida01
                          ( CORRESPONDING #( ls_saida02 )

                           ) ).

  lt_sintetica2 =   VALUE #( FOR ls_saida03 IN lo_report_100->it_saida02
                           ( CORRESPONDING #( ls_saida03 )

                            ) ).
*


  CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
    EXPORTING
      formname           = vl_form
    IMPORTING
      fm_name            = vl_name
    EXCEPTIONS
      no_form            = 1
      no_function_module = 2
      OTHERS             = 3.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

*  Impresora
  ls_control-no_dialog = ' '. "Evita la pantalla de opciones de salida del formulario
  ls_options-tddest   = 'LOCL'.
  ls_options-tdimmed  = 'X'.
  ls_options-tdnewid  = 'X'.
  ls_options-tdnoarch = 'X'.

  ls_control-preview = space.
  ls_control-device  = 'PRINTER'.
  ls_control-getotf  = ' '.

  CLEAR:job_output_info.

  DATA e_ukurs TYPE c LENGTH 10.

  WRITE p_ukurs TO e_ukurs.
  CONDENSE e_ukurs NO-GAPS.

  CALL FUNCTION vl_name
    EXPORTING
      user_settings      = ' '
      control_parameters = ls_control
      output_options     = ls_options
*     i_bukrs            = p_bukrs-low
      i_ukurs            = e_ukurs
      i_datab            = p_dtfim
*     i_ano_viagem       = w_ano
*     i_nr_viagem        = w_viagem
*     i_tp_agrup         = p_tipo
    IMPORTING
      job_output_info    = job_output_info
    TABLES
      it_analitica       = lt_analitica
      it_sintetica       = lt_sintetica
      it_sintetica2      = lt_sintetica2
    EXCEPTIONS
      formatting_error   = 1
      internal_error     = 2
      send_error         = 3
      user_canceled      = 4
      OTHERS             = 5.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
*181777 - CS2022000790 - RGA
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  Z_PREENCHE_BDC
*&---------------------------------------------------------------------*
"  Se Dynbegin = 'X' ele preenche as informações da tela, senão ele preenche
" o campo e o dado dela. prontio.
FORM z_preenche_bdc  USING program
                           dynpro
                           dynbegin
                           fnam
                           fval.

*  IF dynbegin = 'X'.
  MOVE: program     TO st_bdcdata-program,
        dynpro      TO st_bdcdata-dynpro,
        dynbegin    TO st_bdcdata-dynbegin,
        fnam        TO st_bdcdata-fnam,
        fval        TO st_bdcdata-fval.
  APPEND st_bdcdata TO it_bdcdata.

*  ENDIF.
  "  prepara a estrutura para o Loop.
  CLEAR st_bdcdata.
ENDFORM.
