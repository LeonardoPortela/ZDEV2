************************************************************************
* PROJETO            : TGG                                             *
* PROGRAMA           : ZHCMR_PA0078                                    *
* TRANSACAO          : xxxxxxxx                                        *
* DESCRICAO          : Contrato de Trabalho                            *
* xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx *
* AUTOR              : Henrique Martins                                *
*                             *
* DATA               : 29/08/2022                                      *
*----------------------------------------------------------------------*
*                      HISTORICO DE MUDANÇAS                           *
*----------------------------------------------------------------------*
*   DATA   |  AUTOR   |   REQUEST   |           DESCRICAO              *
*----------------------------------------------------------------------*
REPORT zhcmr_pa0078.

***********************************************
* Initialization
***********************************************
INITIALIZATION.
***********************************************
* Start of selection
***********************************************

*======================================================================*
*** Nodes BDL
*======================================================================*
  "NODES: PERAS.

*======================================================================*
*** Tabelas
*======================================================================*
  TABLES: pernr.

  DATA: vl_formname      TYPE tdsfname.

  DATA: it_saida TYPE TABLE OF zhcms_termo_contrato,
        zrua     TYPE addr1_val-street,
        znumero  TYPE addr1_val-house_num1,
        zbairro  TYPE addr1_val-city2,
        wa_saida TYPE zhcms_termo_contrato.

  DATA: v_formname TYPE tdsfname VALUE 'ZHCMS_PA0055', "'ZHCMS_PA0001',
        v_name     TYPE rs38l_fnam.

  DATA: gb_no_open  TYPE c LENGTH 1,
        gb_no_close TYPE c LENGTH 1,
        ck_ultimo   TYPE c LENGTH 1,
        qtd_linhas  TYPE i,
        l_safra_ini TYPE numc4,
        l_safra_fim TYPE numc4.

  DATA: desc       LIKE spell,
        entry_date TYPE dats,
        dias       TYPE vtbbewe-atage,
        vdias      TYPE zhcms_termo_contrato-vlr_salario.

  DATA: BEGIN OF descricao,
          word        LIKE spell-word,
          real(6)     TYPE c,
          filler(3)   TYPE c,
          decimal     LIKE spell-decword,
          centavos(8),
        END OF descricao.

*  DATA: LS_JOB_INFO_AUX   TYPE SSFCRESCL,
*        LS_JOB_INFO       TYPE SSFCRESCL,
*        LS_OUTPUT_OPTIONS TYPE SSFCOMPOP.

*======================================================================*
*** Infotipos
*======================================================================*
  INFOTYPES: 0001 NAME p0001.
  INFOTYPES: 0002 NAME p0002.
  INFOTYPES: 0008 NAME p0008.
  INFOTYPES: 0016 NAME p0016.
  INFOTYPES: 0465 NAME p0465.
  INFOTYPES: 0015 NAME p0015.


*&---------------------------------------------------------------------*
*& SELECTION-SCREEN
*&---------------------------------------------------------------------*

  SELECTION-SCREEN BEGIN OF BLOCK 1 WITH FRAME TITLE text-001.
  SELECTION-SCREEN BEGIN OF LINE.
  PARAMETERS: p_pa_te  AS CHECKBOX USER-COMMAND p_pa_te_1. " Título de Experiência
  SELECTION-SCREEN COMMENT 2(50) text-002 FOR FIELD p_pa_te.
  SELECTION-SCREEN END OF LINE.

  SELECTION-SCREEN BEGIN OF LINE.
  PARAMETERS: p_pa_td  AS CHECKBOX USER-COMMAND p_pa_td_1. " Contrato de Tempo Determinado
  SELECTION-SCREEN COMMENT 2(50) text-003 FOR FIELD p_pa_td.
  SELECTION-SCREEN END OF LINE.

  SELECTION-SCREEN BEGIN OF LINE.
  PARAMETERS: p_pa_ad  AS CHECKBOX USER-COMMAND p_pa_td_1. " Aditamento de contrato
  SELECTION-SCREEN COMMENT 2(50) text-004 FOR FIELD p_pa_ad.
  SELECTION-SCREEN END OF LINE.

  PARAMETERS: p_chave TYPE char30 NO-DISPLAY. "Parâmetro que é preenchido quando chamado pelo report ZHCM_HRST_18_PA_COCKPIT_FORM

  SELECTION-SCREEN END OF BLOCK 1.


START-OF-SELECTION.
* Get objects
  "GET PERAS.

GET pernr.

  rp_provide_from_last p0001 space pn-begda pn-endda.
  rp_provide_from_last p0002 space pn-begda pn-endda.
  rp_provide_from_last p0465 space pn-begda pn-endda.
  rp_provide_from_last p0015 space pn-begda pn-endda.

*-CS2020001417 - 23.12.2020 - inicio
  PERFORM z_seleciona_dados.
*-CS2020001417 - 23.12.2020 - fim

* PERFORM: form_imprimir.

*-CS2020001417 - 23.12.2020 - inicio
END-OF-SELECTION.

  DESCRIBE TABLE it_saida LINES qtd_linhas.

  LOOP AT it_saida INTO wa_saida.

    IF sy-tabix  = qtd_linhas.
      gb_no_open   =  abap_true.
      gb_no_close  =  abap_false.
    ENDIF.

    IF qtd_linhas = 1.
      gb_no_open   =  abap_false.
      gb_no_close  =  abap_false.
    ENDIF.

    PERFORM f_imprimir_novo USING gb_no_open gb_no_close.
  ENDLOOP.
*-CS2020001417 - 23.12.2020 - fim

*******************************************************
* selecao dados
*******************************************************
FORM z_seleciona_dados.

  DATA: it_addr TYPE addr1_val,
        name    LIKE bapibranch-name,
        it_t247 TYPE TABLE OF t247.


  SELECT SINGLE * FROM pa0001
    INTO @DATA(wa_0001)
    WHERE pernr IN @pnppernr
  AND   plans <> '99999999'.

  "Empresa e Filial
  CALL FUNCTION 'HR_BR_GET_FILIAL_PER_AREA'
    EXPORTING
      p_werks        = wa_0001-werks
      p_btrtl        = wa_0001-btrtl
    IMPORTING
      bukrs          = wa_0001-bukrs
      branch         = wa_0001-werks
    EXCEPTIONS
      no_link_areas  = 1
      no_group_found = 2.

  CALL FUNCTION 'HR_BR_LER_FILIAL_GERAL'
    EXPORTING
      company_code      = wa_0001-bukrs
      branch            = wa_0001-werks
      date              = pn-endda
    IMPORTING
      cgc               = wa_saida-cgc_nr
      comp_name         = name
      comp_addr         = it_addr
    EXCEPTIONS
      branch_not_found  = 1
      address_not_found = 2
      company_not_found = 3
      OTHERS            = 4.

  wa_saida-bukrs       = wa_0001-bukrs.
  wa_saida-desc_emp    = it_addr-name1.
  wa_saida-cidade      = it_addr-city1.
  wa_saida-uf          = it_addr-po_box_reg.
  wa_saida-ano         = sy-datum+0(4).
  wa_saida-desc_filial = name.

  zrua                 = it_addr-street.
  znumero              = it_addr-house_num1.
  zbairro              = it_addr-city2.


  SELECT SINGLE *
    FROM pa0002 INTO @DATA(wa_0002)
  WHERE pernr EQ @pernr-pernr.

  wa_saida-cname = wa_0002-cname.


  SELECT *
    FROM pa0465 INTO TABLE @DATA(it_0465)
  WHERE pernr EQ @pernr-pernr.

  LOOP AT it_0465 INTO DATA(wa_0465).

*-CS2020001417 - 23.12.2020 - inicio
    IF wa_0465-subty EQ '0003'.
*     wa_saida-ctps_nr = wa_0465-ctps_nr && wa_0465-ctps_serie.
      wa_saida-ctps_nr    = wa_0465-ctps_nr.
      wa_saida-ctps_serie = wa_0465-ctps_serie.
    ENDIF.
*-CS2020001417 - 23.12.2020 - fim

    IF wa_0465-subty EQ '0002'.
      wa_saida-rg = wa_0465-ident_nr.
    ENDIF.

    IF wa_0465-subty EQ '0001'.
      wa_saida-cpf = wa_0465-cpf_nr.
    ENDIF.
  ENDLOOP.


  SELECT SINGLE * FROM hrp1000 INTO @DATA(wa_hrp1000)
  WHERE objid EQ @wa_0001-plans.


  wa_saida-funcao = wa_hrp1000-stext.


  SELECT SINGLE * FROM pa0007
    INTO @DATA(wa_0007)
  WHERE pernr EQ @pernr-pernr.

  SELECT SINGLE * FROM t508a INTO @DATA(wa_t508a)
    WHERE schkz EQ @wa_0007-schkz
    AND   zeity EQ '1'
    AND   mosid EQ '37'
  AND   mofid EQ 'BR'.


  SELECT SINGLE * FROM t551a INTO @DATA(wa_t551a)
    WHERE zmodn EQ @wa_t508a-zmodn
  AND   motpr = '37'.


  SELECT SINGLE * FROM t550a INTO @DATA(wa_t550a)
    WHERE tprog EQ @wa_t551a-tprg1
  AND   motpr = '37'.

  wa_saida-ini_hora = wa_t550a-sobeg.
  wa_saida-fim_hora = wa_t550a-soend.


  SELECT SINGLE * FROM t550p INTO @DATA(wa_t550p)
    WHERE pamod EQ @wa_t550a-pamod
  AND   motpr EQ '37'.

  wa_saida-dura_intervalo = wa_t550p-pdunb.

  SELECT SINGLE * FROM pa0008 INTO @DATA(wa_0008)
    WHERE pernr EQ @pernr-pernr
  AND   subty NE 'BR01'.

  wa_saida-vlr_salario = wa_0008-bet01.


  CALL FUNCTION 'SPELL_AMOUNT'
    EXPORTING
      amount    = wa_saida-vlr_salario
      currency  = 'BRL'
      filler    = space
      language  = sy-langu
    IMPORTING
      in_words  = desc
    EXCEPTIONS
      not_found = 1
      too_large = 2
      OTHERS    = 3.


  IF wa_saida-vlr_salario > 1.
    descricao-real = 'REAIS'.
    descricao-word = desc-word.
  ELSE.
    descricao-real = 'REAL'.
    descricao-word = desc-word.
  ENDIF.

  IF desc-decword NE '' OR desc-decword = '0'.
    descricao-filler = 'E'.
    descricao-decimal = desc-decword.
    descricao-centavos = 'CENTAVOS'.
  ENDIF.

  CONDENSE descricao.

  CLEAR desc.

  wa_saida-desc_salario = descricao.

  SELECT SINGLE * FROM pa0016 INTO @DATA(wa_0016)
    WHERE pernr EQ @pernr-pernr
  AND   endda EQ '99991231'.

*-CS2020001417 - 23.12.2020 - inicio
  CALL FUNCTION 'HR_ENTRY_DATE'
    EXPORTING
      persnr    = pernr-pernr
      begda     = '18000101'
      endda     = '99991231'
    IMPORTING
      entrydate = entry_date.

  l_safra_ini = entry_date(4) - 1.
  l_safra_fim = entry_date(4).
  CONCATENATE l_safra_ini '/' l_safra_fim
         INTO wa_saida-safra_soja.
*-CS2020001417 - 23.12.2020 - fim

  IF wa_0016-ctedt IS NOT INITIAL.

*   CALL FUNCTION 'HR_ENTRY_DATE'
*     EXPORTING
*       persnr    = pernr-pernr
*       begda     = '18000101'
*       endda     = '99991231'
*     IMPORTING
*       entrydate = entry_date.

    CONCATENATE entry_date+6(2)    '/'  entry_date+4(2)    '/' entry_date+0(4)    INTO  wa_saida-dt_inicio.
    CONCATENATE wa_0016-ctedt+6(2) '/'  wa_0016-ctedt+4(2) '/' wa_0016-ctedt+0(4) INTO  wa_saida-dt_fim.

    dias = ( wa_0016-ctedt -  entry_date ).
    wa_saida-prazo_dias = dias + 1.
    vdias =  wa_saida-prazo_dias.

    CALL FUNCTION 'SPELL_AMOUNT'
      EXPORTING
        amount    = vdias
        currency  = 'BRL'
        filler    = space
        language  = sy-langu
      IMPORTING
        in_words  = desc
      EXCEPTIONS
        not_found = 1
        too_large = 2
        OTHERS    = 3.

    wa_saida-desc_dias = desc-word.

  ENDIF.

  SELECT SINGLE * FROM t247 INTO @DATA(wa_t247)
     WHERE spras EQ @sy-langu
  AND   mnr   EQ @entry_date+4(2).

  CONCATENATE wa_saida-cidade '-' wa_saida-uf  entry_date+6(2) 'de'  wa_t247-ltx 'de'  entry_date+0(4) INTO wa_saida-dt_desc SEPARATED BY space.

  APPEND wa_saida TO it_saida.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  FORM_IMPRIMIR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM form_imprimir.

  DATA: vl_formname  TYPE tdsfname,
        vl_name      TYPE rs38l_fnam,
*---DADOS EMPRESA
        wa_branch    TYPE bapibranch-branch,
        return       LIKE bapiret2 OCCURS 0 WITH HEADER LINE,
        g_branchlist LIKE bapibranch OCCURS 0 WITH HEADER LINE,
        "V_BUKRS_TEXT TYPE T001-BUTXT,
        v_bukrs_text TYPE adrc-name1,
        ls_text      LIKE p1000-stext,
        p0041_line   TYPE pa0041,
        v_cidade     TYPE string,
        v_ort01      LIKE lfa1-ort01,
        v_regio      LIKE lfa1-regio,
        v_stras      LIKE lfa1-stras,
        v_dias       LIKE p0041-dar04,
        v_prorrog    LIKE p0041-dar04,
        v_dataad     LIKE p0000-begda.

  CASE abap_true.
    WHEN p_pa_te.
      " H - Nome do smartform que vai ser gerado.
      vl_formname = 'ZHCMS_PA0028'.
    WHEN p_pa_td.
      vl_formname = ''.
  ENDCASE.

  CALL FUNCTION 'HR_ENTRY_DATE'
    EXPORTING
      persnr    = pernr-pernr
      begda     = '18000101'
      endda     = '99991231'
    IMPORTING
      entrydate = v_dataad.


  SELECT SINGLE * FROM j_1bbranch INTO @DATA(wa_1bbranch)
  WHERE branch EQ @p0001-werks.

  SELECT SINGLE * FROM adrc  INTO @DATA(wadrc)
  WHERE addrnumber EQ @wa_1bbranch-adrnr.

  v_bukrs_text = wadrc-name1.

  CONCATENATE  wadrc-street ',' wadrc-house_num1  INTO v_stras SEPARATED BY space.
  CONCATENATE  wadrc-city1  '-' wadrc-region      INTO v_cidade SEPARATED BY space.

  TRANSLATE v_bukrs_text TO UPPER CASE.


*  CALL FUNCTION 'HRWPC_RFC_BUKRS_TEXT_GET'
*    EXPORTING
*      BUKRS      = P0001-BUKRS
*      LANGU      = SY-LANGU
*    IMPORTING
*      BUKRS_TEXT = V_BUKRS_TEXT.

  CALL FUNCTION 'HR_BR_GET_FILIAL_PER_AREA'
    EXPORTING
      p_werks = p0001-werks
      p_btrtl = p0001-btrtl
    IMPORTING
      branch  = wa_branch.

  CALL FUNCTION 'BAPI_BRANCH_GETLIST'
    EXPORTING
      company     = p0001-bukrs
      branch      = wa_branch
    TABLES
      branch_list = g_branchlist
      return      = return.

  " Dados da empresa
  DATA: v_lifnr LIKE lfa1-lifnr.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = g_branchlist-branch
    IMPORTING
      output = v_lifnr.

*  SELECT SINGLE ORT01 REGIO " STRAS
*    FROM LFA1
*    "INTO (V_ORT01, V_REGIO, V_STRAS)
*    INTO (V_ORT01, V_REGIO)
*    WHERE LIFNR = V_LIFNR.
*
*  CONCATENATE V_ORT01 '-' V_REGIO INTO V_CIDADE SEPARATED BY SPACE.


  CALL FUNCTION 'HR_READ_FOREIGN_OBJECT_TEXT'
    EXPORTING
      otype                   = 'S'
      objid                   = p0001-plans
      begda                   = p0001-begda
      endda                   = p0001-endda
    IMPORTING
      object_text             = ls_text
    EXCEPTIONS
      nothing_found           = 1
      wrong_objecttype        = 2
      missing_costcenter_data = 3
      missing_object_id       = 4
      OTHERS                  = 5.
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.

* Get Date Specifications
  SELECT SINGLE *
           FROM pa0041
           INTO p0041_line
          WHERE pernr = p0001-pernr AND
  endda = '99991231'.

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

  DATA: wa_control_parameters TYPE ssfctrlop,
        wa_output_options     TYPE ssfcompop,

        print_co              LIKE  print_co,
        print_opts            LIKE  itcpo.



*  CALL FUNCTION 'CO_PRINT_GET_INFO_LIST'
*    IMPORTING
*      PRINT_CO_EXP   = PRINT_CO
*      PRINT_OPTS_EXP = PRINT_OPTS
*    EXCEPTIONS
*      OTHERS         = 0.

*  WA_CONTROL_PARAMETERS-DEVICE = 'PRINTER'.
*  WA_CONTROL_PARAMETERS-PREVIEW = ''.
*  WA_CONTROL_PARAMETERS-NO_DIALOG = 'X'.
*  WA_OUTPUT_OPTIONS-TDDEST = PRINT_CO-DESTI.
**    wa_output_options-tdprinter = print_co-desti.
*  WA_OUTPUT_OPTIONS-TDARMOD = '1'.
*  WA_OUTPUT_OPTIONS-TDCOPIES = '001'.
*  WA_OUTPUT_OPTIONS-TDDELETE = 'X'.
*  WA_OUTPUT_OPTIONS-TDLIFETIME = '2'.
*  WA_OUTPUT_OPTIONS-TDTITLE = PRINT_OPTS-TDTITLE.

  IF p_chave IS INITIAL.
    wa_output_options-tdcovtitle = 'Monthly Performence Report'(t01).
    wa_output_options-tdnewid = space.        " New Spool request
    wa_output_options-tdimmed = 'X'.          " Print Immeditely
    wa_output_options-tdcopies = '1'.         " No of copies
    wa_output_options-tddest = sy-pdest.      " Printer name
  ELSE.
    wa_output_options-tdcovtitle = p_chave.
  ENDIF.

  wa_control_parameters-no_dialog = 'X'.

  CLEAR: v_dias,v_prorrog.
  v_dias    = ( p0041_line-dat03 - p0041_line-dat01 ) + 1. "Dias Contrato.
  v_prorrog = ( p0041_line-dat04 - p0041_line-dat03 ). "Prorrogação Contrato

  CALL FUNCTION vl_name
    EXPORTING
      p_pernr            = p0001-pernr
      p_dataadm          = v_dataad
      name               = p0002-cname  "Nome do funcionário
      bukrs_text         = v_bukrs_text "Nome da Empresa
      branch_name        = g_branchlist-name
      bukrs_end          = v_stras
      control_parameters = wa_control_parameters
      bukrs_city         = v_cidade "Cidade
      plans_text         = ls_text
      per_dar03          = v_dias     "P0041_LINE-DAR03 "Contrato dias.
      per_dar04          = v_prorrog  "P0041_LINE-DAR04 "Prorrogação
      output_options     = wa_output_options
      user_settings      = ' '
      rua                = zrua
      numero             = znumero
      bairro             = zbairro
    TABLES
      p_0465             = p0465[]
      p_0001             = p0001[]
    EXCEPTIONS
      formatting_error   = 1
      internal_error     = 2
      send_error         = 3
      user_canceled      = 4
      OTHERS             = 5.

  IF sy-subrc <> 0.
    "DO NOTHING
  ENDIF.

*  CALL FUNCTION '/1BCDWB/SF00000155'
*    EXPORTING
**     ARCHIVE_INDEX              =
**     ARCHIVE_INDEX_TAB          =
**     ARCHIVE_PARAMETERS         =
**     CONTROL_PARAMETERS         =
**     MAIL_APPL_OBJ              =
**     MAIL_RECIPIENT             =
**     MAIL_SENDER =
**     OUTPUT_OPTIONS             =
**     USER_SETTINGS              = 'X'
*      NAME        =
*      BUKRS_TEXT  =
*      BRANCH_NAME =
*      BUKRS_END   =
*      BUKRS_CITY  =
*      PLANS_TEXT  =
*      PER_DAR03   =
*      PER_DAR04   =
*      P_PERNR     =
** IMPORTING
**     DOCUMENT_OUTPUT_INFO       =
**     JOB_OUTPUT_INFO            =
**     JOB_OUTPUT_OPTIONS         =
*    TABLES
*      P_0465      =
** EXCEPTIONS
**     FORMATTING_ERROR           = 1
**     INTERNAL_ERROR             = 2
**     SEND_ERROR  = 3
**     USER_CANCELED              = 4
**     OTHERS      = 5
*    .
*  IF SY-SUBRC <> 0.
** Implement suitable error handling here
*  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  IMPRIMIR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GB_NO_OPEN  text
*      -->P_GB_NO_CLOSE  text
*----------------------------------------------------------------------*
FORM f_imprimir_novo  USING    p_no_open
                               p_no_close.

  DATA: lc_control_parameters TYPE  ssfctrlop.

  DATA: wa_output_options     TYPE ssfcompop.

  IF p_chave IS INITIAL.
    lc_control_parameters-no_open  = p_no_open.
    lc_control_parameters-no_close = p_no_close.
  ELSE.
    wa_output_options-tdcovtitle    = p_chave.
    wa_output_options-tdimmed       = space.
    lc_control_parameters-no_dialog = 'X'.
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
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    EXIT.
  ENDIF.

  CALL FUNCTION v_name
    EXPORTING
      control_parameters = lc_control_parameters
      wa_saida           = wa_saida
      output_options     = wa_output_options
      user_settings      = ' '
      rua                = zrua
      numero             = znumero
      bairro             = zbairro
    EXCEPTIONS
      formatting_error   = 1
      internal_error     = 2
      send_error         = 3
      user_canceled      = 4
      OTHERS             = 5.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    EXIT.
  ENDIF.

ENDFORM.
