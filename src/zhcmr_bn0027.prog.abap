************************************************************************
* PROJETO            : MRS                                             *
* PROGRAMA           : ZHCMR_BN0027                                    *
* TRANSACAO          :                                                 *
* DESCRICAO          : DCI no KIT Admissional
* xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx *
* AUTOR              : Rubenilson Pereira                              *
* Solicitante        : Nome do solicitante                             *
* DATA               : 17/05/2024                                      *
*----------------------------------------------------------------------*
*                      HISTORICO DE MUDANÇAS                           *
*----------------------------------------------------------------------*
*   DATA   |  AUTOR   |   REQUEST   |           DESCRICAO              *
*----------------------------------------------------------------------*
REPORT zhcmr_bn0027.

INITIALIZATION.

*======================================================================*
*** Nodes BDL
*======================================================================*
  NODES: peras.

*======================================================================*
*** Tabelas
*======================================================================*
  TABLES: pernr.

*======================================================================*
*** Infotipos
*======================================================================*
  INFOTYPES: 0001 NAME p0001.
  INFOTYPES: 0002 NAME p0002.
  INFOTYPES: 0006 NAME p0006.
  INFOTYPES: 0465 NAME p0465.

*----------------------------------------------------------------------*
* SELECTION-SCREEN
*----------------------------------------------------------------------*

  PARAMETERS: p_chave TYPE char30 NO-DISPLAY. "Parâmetro que é preenchido quando chamado pelo report ZHCM_HRST_18_PA_COCKPIT_FORM

START-OF-SELECTION.
* Get objects
GET peras.
  rp_provide_from_last p0001 space pn-begda pn-endda.
  rp_provide_from_last p0002 space pn-begda pn-endda.
  rp_provide_from_last p0006 space pn-begda pn-endda.
  rp_provide_from_last p0465 space pn-begda pn-endda.


  PERFORM: form_imprimir.
*&---------------------------------------------------------------------*
*&      Form  ZF_SET_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM form_imprimir.
  " Variaveis para PDF individual, por CPF
  CONSTANTS c_x TYPE c VALUE 'X'.

  STATICS: fm_name TYPE rs38l_fnam,
           fpname  TYPE fpname.

  DATA: vl_form         TYPE tdsfname,
        v_mask_locl(60) TYPE c,
        v_cont          TYPE i,
        v_cpf(11),
        c_cont,
        p_input(60),
        t_dir_local     TYPE TABLE OF sdokpath,
        t_dir_loc_f     TYPE TABLE OF sdokpath.

  "PDF
  DATA: i_otf           TYPE itcoo OCCURS 0 WITH HEADER LINE,
        i_tline         TYPE TABLE OF tline WITH HEADER LINE,
        v_bin_filesize  TYPE i,
        i_record        LIKE solisti1 OCCURS 0 WITH HEADER LINE,
        wa_buffer       TYPE string,
        lv_nome_arquivo TYPE string,
        filename        LIKE rlgrap-filename,
        lv_nome         TYPE string,
        ls_Dados        TYPE zhcm_form_dci.

  DATA: ls_control      TYPE ssfctrlop,
        ls_options2     TYPE ssfcompop,
        job_output_info TYPE ssfcrescl,
        v_bol           TYPE abap_bool.

  TYPES: BEGIN OF ty_saida,
           cname  TYPE zhcms_bn0008-cname,
           cpf_nr TYPE zhcms_bn0008-cpf_nr,
           gbdat  TYPE zhcms_bn0008-gbdat,
           gesch  TYPE zhcms_bn0008-gesch,
           famst  TYPE zhcms_bn0008-famst,
           datadm TYPE zhcms_bn0008-datadm,
           stras  TYPE zhcms_bn0008-stras,
           hsnmr  TYPE zhcms_bn0008-hsnmr,
           posta  TYPE zhcms_bn0008-posta,
           ort02  TYPE zhcms_bn0008-ort02,
           ort01  TYPE zhcms_bn0008-ort01,
           state  TYPE zhcms_bn0008-state,
           pstlz  TYPE zhcms_bn0008-pstlz,
           estcv  TYPE zhcms_bn0008-estcv,
           funcao TYPE zhcms_bn0008-funcao,
           local  TYPE zhcms_bn0008-local,
         END OF ty_saida.


  DATA:
    it_0465  TYPE TABLE OF p0465,
    wa_0465  LIKE LINE OF it_0465,
    t_saida  TYPE TABLE OF ty_saida,
    wa_saida LIKE LINE OF t_saida.

  DATA: vl_formname  TYPE tdsfname,
        vl_name      TYPE rs38l_fnam,
        lva_dt_admis TYPE sy-datum.

  DATA: wa_control_parameters TYPE ssfctrlop,
        wa_output_options     TYPE ssfcompop.


  READ TABLE p0002 ASSIGNING FIELD-SYMBOL(<fs_pa0002>) INDEX 1.
  IF sy-subrc IS INITIAL.
    ls_dados-cname = <fs_pa0002>-cname.
  ENDIF.

*  SELECT cname
*    FROM pa0002
*    INTO ls_dados-cname
*    UP TO 1 ROWS
*    WHERE pernr = p_pernr.
*  ENDSELECT.

  READ TABLE p0001 ASSIGNING FIELD-SYMBOL(<fs_pa0001>) INDEX 1.
  IF sy-subrc IS INITIAL.
    ls_dados-stell = <fs_pa0001>-stell.
    ls_dados-werks = <fs_pa0001>-werks.
  ENDIF.
*  SELECT stell werks
*    FROM pa0001
*    INTO (ls_dados-stell,ls_dados-werks)
*    UP TO 1 ROWS
*    WHERE pernr = p_pernr
*      AND begda <= pn-endda
*      AND endda >= pn-endda.
*  ENDSELECT.
  IF ls_dados-stell IS NOT INITIAL.
    SELECT stext
      FROM hrp1000
      INTO ls_dados-stext
      UP TO 1 ROWS
      WHERE objid = ls_dados-stell
        AND otype = 'C'
        AND plvar = '01'
        AND endda >= sy-datum
        AND langu = sy-langu.
    ENDSELECT.
  ENDIF.

  IF ls_dados-werks IS NOT INITIAL.

    SELECT SINGLE name1
      FROM t001w
      INTO ls_dados-name1
      WHERE werks = ls_dados-werks.

  ENDIF.

  " Por CPF
*  Impresora
  ls_control-no_dialog = c_x.
  ls_options2-tddest   = 'LOCL'.
  ls_options2-tdimmed  = c_x.
  ls_options2-tdnewid  = c_x.
  ls_options2-tdnoarch = c_x.
  ls_options2-tdnoprev = c_x.

  ls_control-preview = space.
  ls_control-device  = 'PRINTER'.
  ls_control-getotf  = c_x.

  CLEAR:job_output_info.

  vl_formname = 'ZHCM_DECLAR_CONFLITO_INTERESSE'.

  CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
    EXPORTING
      formname           = vl_formname
    IMPORTING
      fm_name            = vl_name
    EXCEPTIONS
      no_form            = 1
      no_function_module = 2
      OTHERS             = 3.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    EXIT.
  ENDIF.

  IF p_chave IS NOT INITIAL.
    wa_output_options-tdcovtitle    = p_chave.
    wa_output_options-tdimmed       = space.
    wa_control_parameters-no_dialog = 'X'.
  ENDIF.

  CALL FUNCTION vl_name
    EXPORTING
      control_parameters = wa_control_parameters
      output_options     = wa_output_options
      user_settings      = 'X'
      dados              = ls_dados
    EXCEPTIONS
      formatting_error   = 1
      internal_error     = 2
      send_error         = 3
      user_canceled      = 4
      OTHERS             = 5.

  IF sy-subrc NE 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    EXIT.
  ENDIF.


ENDFORM.
