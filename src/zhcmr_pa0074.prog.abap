*&--------------------------------------------------------------------&*
*&                         Consultoria                                &*
*&--------------------------------------------------------------------&*
*& Projeto..: AMAGGI                                                  &*
*& Autor....: CAMILA BRAND                                            &*
*& Data.....: 01/02/2022                                              &*
*& Descrição:  TERMO CARTILHAS BENEFÍCIOS, CÓD ÉTICA E PR.SEGURAS     &*
*& Transação:                                                         &*
*&--------------------------------------------------------------------&*
*& Projeto  :                                                         &*
*& Código Espec.Funcional/Técnica:                                    &*
*&--------------------------------------------------------------------&*
*&                    Histórico de Modificações                       &*
*& Autor           Request      Data         Descrição                &*
*& ABAP                                                               &*
*&--------------------------------------------------------------------&*
REPORT zhcmr_pa0074.
*----------------------------------------------------------------------*
* NODES BDL
*----------------------------------------------------------------------*
NODES: peras.

*======================================================================*
*** Tabelas
*======================================================================*
TABLES: pernr.

*----------------------------------------------------------------------*
* INFOTIPOS
*----------------------------------------------------------------------*
INFOTYPES: 0001 NAME p0001.
INFOTYPES: 0002 NAME p0002.


SELECTION-SCREEN BEGIN OF BLOCK 1 .
PARAMETERS: p_chave TYPE char30 NO-DISPLAY. "Parâmetro que é preenchido quando chamado pelo report ZHCM_HRST_18_PA_COCKPIT_FORM
SELECTION-SCREEN END OF BLOCK 1.

*----------------------------------------------------------------------*
* SELECTION-SCREEN
*----------------------------------------------------------------------*

START-OF-SELECTION.
* Get objects
GET peras.

  rp_provide_from_last p0001 space pn-begda pn-endda.
  rp_provide_from_last p0002 space pn-begda pn-endda.

  PERFORM: form_imprimir.
*&---------------------------------------------------------------------*
*&      Form  FORM_IMPRIMIR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM form_imprimir .

  DATA: vl_formname    TYPE tdsfname,
        vl_name        TYPE rs38l_fnam,

*---DADOS EMPRESA
        wa_branch      TYPE bapibranch-branch,
        return         LIKE bapiret2 OCCURS 0 WITH HEADER LINE,
        g_branchlist   LIKE bapibranch OCCURS 0 WITH HEADER LINE,
        v_bukrs_text   TYPE t001-butxt,
        v_werks_text   TYPE t500p-name1,
        v_dataad       TYPE p0001-begda,
        ls_text        LIKE p1000-stext,
        lva_stext      TYPE hrp1000-stext,
        lva_cargo(100) TYPE c.


  DATA: wa_control_parameters TYPE ssfctrlop,
        wa_output_options     TYPE ssfcompop.

  vl_formname = 'ZHCMS_PA0025'.

  CALL FUNCTION 'HRWPC_RFC_BUKRS_TEXT_GET'
    EXPORTING
      bukrs      = p0001-bukrs
      langu      = sy-langu
    IMPORTING
      bukrs_text = v_bukrs_text.

  CALL FUNCTION 'HRWPC_RFC_WERKS_TEXT_GET'
    EXPORTING
      werks      = p0001-werks
    IMPORTING
      werks_text = v_werks_text.


  CALL FUNCTION 'HR_ENTRY_DATE'
    EXPORTING
      persnr    = pernr-pernr
      begda     = '18000101'
      endda     = '99991231'
    IMPORTING
      entrydate = v_dataad.



  CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
    EXPORTING
      formname           = vl_formname
    IMPORTING
      fm_name            = vl_name
    EXCEPTIONS
      no_form            = 1
      no_function_module = 2
      OTHERS             = 3.

  CLEAR: lva_cargo , lva_stext.
  SELECT SINGLE stext FROM hrp1000
  INTO lva_stext
   WHERE plvar = '01'
     AND otype = 'C'
     AND objid = p0001-stell
     AND langu = sy-langu.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    EXIT.
  ENDIF.

  CONCATENATE p0001-stell '-' lva_stext INTO lva_cargo SEPARATED BY space.

  IF p_chave IS NOT INITIAL.
    wa_output_options-tdcovtitle    = p_chave.
    wa_output_options-tdimmed       = space.
    wa_control_parameters-no_dialog = 'X'.
  ENDIF.

  CALL FUNCTION vl_name
    EXPORTING
      name               = p0002-cname  "Nome do funcionário
      pernr              = p0001-pernr  "Número da Chapa
      werks_text         = v_werks_text
      stell              = lva_cargo
      bukrs_text         = v_bukrs_text "Nome da Empresa
      p_dataadm          = v_dataad
      control_parameters = wa_control_parameters
      output_options     = wa_output_options
      user_settings      = ' '
    EXCEPTIONS
      formatting_error   = 1
      internal_error     = 2
      send_error         = 3
      user_canceled      = 4
      OTHERS             = 5.

  IF sy-subrc <> 0.
  ENDIF.

ENDFORM.
