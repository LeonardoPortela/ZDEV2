************************************************************************
* PROJETO            : TGG                                             *
* PROGRAMA           : ZHCMR_PA0079                                    *
* TRANSACAO          : xxxxxxxx                                        *
* DESCRICAO          : Contrato de Trabalho Determinado                *
* xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx *
* AUTOR              : Henrique Martins                                *
*                             *
* DATA               : 29/08/2022                                      *
*----------------------------------------------------------------------*
*                      HISTORICO DE MUDANÇAS                           *
*----------------------------------------------------------------------*
*   DATA   |  AUTOR   |   REQUEST   |           DESCRICAO              *
*----------------------------------------------------------------------*
REPORT zhcmr_pa0079.


*======================================================================*
*** Infotipos
*======================================================================*

SELECTION-SCREEN BEGIN OF BLOCK 1.
PARAMETERS: p_chave TYPE char30 NO-DISPLAY.
SELECTION-SCREEN END OF BLOCK 1.

DATA: it_saida         TYPE TABLE OF zhcms_termo_contrato,
      wa_dados_empresa TYPE zhcms_reg_empregado,
      bukrs            LIKE bapibranch-bukrs,
      branch           LIKE bapibranch-branch,
      name             LIKE bapibranch-name,
      cgc_nr           LIKE bapibranch-cgc_number,
      zrua             TYPE addr1_val-street,
      znumero          TYPE addr1_val-house_num1,
      zbairro          TYPE addr1_val-city2,
      wa_saida         TYPE zhcms_termo_contrato.

DATA: v_formname TYPE tdsfname VALUE 'ZHCMS_PA0029',
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

INITIALIZATION.

  NODES: peras.

  TABLES: pernr.


START-OF-SELECTION.

GET peras.

  PERFORM z_seleciona_dados.


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

    PERFORM imprimir USING gb_no_open gb_no_close.
  ENDLOOP.


FORM z_seleciona_dados.

  DATA: it_addr TYPE addr1_val,
        name    LIKE bapibranch-name,
        it_t247 TYPE TABLE OF t247.


  SELECT SINGLE * FROM pa0001
    INTO @DATA(wa_0001)
    WHERE pernr IN  @pnppernr
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

*
*  "Dados da empresa.
*  "Endereço e CGC
*  CALL FUNCTION 'HR_BR_LER_FILIAL_GERAL'
*    EXPORTING
*      company_code      = bukrs
*      branch            = branch
*      date              = pn-endda
*    IMPORTING
*      cgc               = cgc_nr
*      comp_name         = name
*      comp_addr         = it_addr
*    EXCEPTIONS
*      branch_not_found  = 1
*      address_not_found = 2
*      company_not_found = 3
*      OTHERS            = 4.
*
*  wa_dados_empresa-name1 = it_addr-name1.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  IMPRIMIR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GB_NO_OPEN  text
*      -->P_GB_NO_CLOSE  text
*----------------------------------------------------------------------*
FORM imprimir  USING    p_no_open
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
      bairro            = zbairro
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
