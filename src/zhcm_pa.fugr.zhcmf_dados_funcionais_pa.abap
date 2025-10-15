FUNCTION zhcmf_dados_funcionais_pa.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(STAT2) LIKE  P0000-STAT2 OPTIONAL
*"     VALUE(PERNR) LIKE  P0001-PERNR OPTIONAL
*"     VALUE(CNAME) LIKE  PA0002-CNAME OPTIONAL
*"     VALUE(ENDDA) LIKE  P0001-ENDDA
*"  TABLES
*"      T_SAIDA STRUCTURE  ZHCMS_FUNC_LIST_PA
*"      T_OBJID STRUCTURE  ZHCMS_OBJID OPTIONAL
*"      T_AREARH STRUCTURE  ZHCMS_WERKS OPTIONAL
*"----------------------------------------------------------------------

  " SEMPRE - > Buscar sempre a delimitação mais recente do infotipo 0001
  " acessar o infotipo 0000 no campo STAT2 = STAT2

*&---------------------------------------------------------------------*
*& TYPES
*&---------------------------------------------------------------------*
  TYPES:

    BEGIN OF ty_pa0001,
      pernr TYPE  pa0001-pernr,
      cname TYPE  pa0002-cname,
      bukrs TYPE  pa0001-bukrs,
      werks TYPE  pa0001-werks,
      btrtl TYPE  pa0001-btrtl,
      orgeh TYPE  pa0001-orgeh,
      stat2 TYPE  pa0000-stat2,
      plans TYPE  pa0001-plans,
      kokrs TYPE  pa0001-kokrs,
      kostl TYPE  pa0001-kostl,
      persk TYPE  pa0001-persk,
      abkrs TYPE  pa0001-abkrs,
      gbdat TYPE  pa0002-gbdat,
      endda	TYPE  pa0001-endda,
      begda	TYPE  pa0001-begda,
    END OF ty_pa0001.

  TYPES: ty_rg_objid TYPE RANGE OF p0001-orgeh,
         ty_rg_pernr TYPE RANGE OF p0001-pernr,
         ty_rg_werks TYPE RANGE OF p0001-werks,
         ty_rg_cname TYPE RANGE OF p0002-cname,
         ty_rg_stat2 TYPE RANGE OF p0000-stat2,
         ty_rg_plans TYPE RANGE OF p0001-plans,
         ty_rg_orgeh TYPE RANGE OF p0001-orgeh.


*&---------------------------------------------------------------------*
*& DATA
*&---------------------------------------------------------------------*

  DATA: v_werks_text     TYPE t500p-name1,
        l_it_0465        TYPE TABLE OF p0465,
        l_0465           LIKE LINE OF l_it_0465,
        l_it_0105        TYPE TABLE OF p0105,
        l_0105           LIKE LINE OF l_it_0105,

        l_it_0041        TYPE TABLE OF p0041,
        l_0041           LIKE LINE OF l_it_0041,

        l_it_0007        TYPE TABLE OF p0007,
        l_0007           LIKE LINE OF l_it_0007,

        l_it_2001        TYPE TABLE OF pa2001,
        l_2001           LIKE LINE OF l_it_2001,
        it_result_struc  LIKE struc OCCURS 0 WITH HEADER LINE,
        wa_result_struc  LIKE it_result_struc,
        wa_func_list     LIKE t_saida,
        v_pernr          TYPE  p0002-pernr,
        tl_pa0001        TYPE TABLE OF ty_pa0001,
        wl_pa0001        LIKE LINE OF tl_pa0001,
        wa_branch        TYPE bapibranch-branch,
        t_saida_superior LIKE zhcms_ret_sup9002 OCCURS 0 WITH HEADER LINE,
        it_addr          TYPE addr1_val,
        cgc_nr           LIKE bapibranch-cgc_number,
        name             LIKE bapibranch-name.

  DATA: vl_data TYPE sy-datum.
  DATA: t_pa00          TYPE TABLE OF pa0000.
  DATA: t_pa01          TYPE TABLE OF pa0001.
  DATA: t_pa02          TYPE TABLE OF pa0002.
  DATA: t_t503t         TYPE TABLE OF t503t.
  DATA: t_t549t         TYPE TABLE OF t549t.
  DATA: w_pa00          TYPE pa0000.
  DATA: w_pa01          TYPE pa0001.
  DATA: w_pa02          TYPE pa0002.
  DATA: w_t503t         TYPE t503t.
  DATA: w_t549t         TYPE t549t.



*======================================================================*
*** PREENCHE OS RANGES COM AS INFORMAÇÕES DE ENTRADA
*======================================================================*
  "Se informar OBJID deleta as demais.
  IF t_objid[] IS NOT INITIAL.
    DATA(rg_objid) = VALUE ty_rg_objid( FOR lwa_objid IN t_objid[] (
        sign = 'I' option = 'EQ' low = lwa_objid-objid high = lwa_objid-objid )
    ).
  ENDIF.

  "Se informar PERNR deleta as demais.
  IF pernr IS NOT INITIAL.
    DATA(rg_pernr) = VALUE ty_rg_pernr(
        ( sign = 'I' option = 'EQ' low = pernr ) ).
  ENDIF.

  "Se informar name deleta as demais.
  IF cname IS NOT INITIAL.
    DATA(rg_cname) = VALUE ty_rg_cname(
        ( sign = 'I' option = 'CP' low = |*{ cname }*| ) ).
  ENDIF.

  "Se informar status de ocupação deleta as demais.
  IF stat2 IS NOT INITIAL.
    DATA(rg_stat2) = VALUE ty_rg_stat2(
          ( sign = 'I' option = 'EQ' low = stat2 ) ).
  ENDIF.


  "Se informar Area RH
  IF t_arearh[] IS NOT INITIAL.
    DATA(rg_arearh) = VALUE ty_rg_werks( FOR lwa_werks IN t_arearh[] (
        sign = 'I' option = 'EQ' low = lwa_werks-werks )
    ).
  ENDIF.



  vl_data = endda.

* Infotipo 0001
  SELECT *
    FROM pa0001
    INTO TABLE t_pa01
   WHERE pernr IN rg_pernr AND
         endda GE vl_data AND
         begda LE vl_data AND
         werks IN rg_arearh AND
         orgeh IN rg_objid
         ORDER BY PRIMARY KEY.

  IF sy-subrc EQ 0.
*   Validar status
    SELECT *
      FROM pa0000
      INTO TABLE t_pa00
       FOR ALL ENTRIES IN t_pa01
     WHERE pernr EQ t_pa01-pernr AND
           endda GE vl_data AND
           begda LE vl_data AND
           stat2 IN rg_stat2[]
           ORDER BY PRIMARY KEY.
***    IF sy-subrc EQ 0.
***      DELETE t_pa00 WHERE stat2 NOT IN rg_stat2.
***    ENDIF.

    SELECT * "PERSK PTEXT
         FROM t503t
    INTO TABLE t_t503t
        FOR ALL ENTRIES IN t_pa01
    WHERE sprsl EQ sy-langu
        AND persk = t_pa01-persk .
    IF sy-subrc EQ 0.
*---> 04/07/2023 - Migração S4 - WS
      SORT t_t503t BY persk .
*<--- 04/07/2023 - Migração S4 - WS
      DELETE ADJACENT DUPLICATES FROM t_t503t COMPARING persk .
    ENDIF.

    SELECT *
      FROM t549t
    INTO TABLE t_t549t
     FOR ALL ENTRIES IN t_pa01
    WHERE sprsl EQ sy-langu
     AND abkrs = t_pa01-abkrs .
    IF sy-subrc EQ 0.
      SORT t_t549t[] BY abkrs ASCENDING.
      DELETE ADJACENT DUPLICATES FROM t_t549t COMPARING abkrs .
    ENDIF.

*   Validar nome
    SELECT pernr, cname, gbdat
      FROM pa0002
      INTO TABLE @DATA(lit_pa0002) "  t_pa02
       FOR ALL ENTRIES IN @t_pa01
     WHERE pernr EQ @t_pa01-pernr AND
           endda GE @vl_data AND
           begda LE @vl_data.
    SORT lit_pa0002[] BY pernr ASCENDING.

    t_pa02[] = VALUE #( FOR lwa_0002 IN lit_pa0002[] (
         pernr = lwa_0002-pernr
         cname = to_upper( lwa_0002-cname )
         gbdat = lwa_0002-gbdat  ) ).
    DELETE t_pa02 WHERE cname NOT IN rg_cname.

* Busca documentos de todos os funcionários:
    SELECT pernr, subty, cpf_nr, ident_nr, ident_org, ctps_nr, ctps_serie, pis_nr
       FROM pa0465
       INTO TABLE @DATA(lit_0465)
       WHERE pernr IN @rg_pernr
         AND subty IN ( '0001', '0002', '0003', '0006' )
         AND endda >= @vl_data
         AND begda <= @vl_data
       ORDER BY pernr, subty ASCENDING.

    LOOP AT t_pa01 INTO w_pa01.
      READ TABLE lit_0465[] WITH KEY pernr = w_pa01-pernr subty = '0001'
        BINARY SEARCH TRANSPORTING NO FIELDS.
*     Se não possuir CPF, ignorar linha
      IF ( sy-subrc <> 0 ).
        CONTINUE.
      ENDIF.
      READ TABLE t_pa00 INTO w_pa00 WITH KEY pernr = w_pa01-pernr BINARY SEARCH.
      IF sy-subrc EQ 0.
        READ TABLE t_pa02 INTO w_pa02 WITH KEY pernr = w_pa01-pernr BINARY SEARCH.
        IF sy-subrc EQ 0.
          MOVE-CORRESPONDING w_pa01 TO wl_pa0001.
          wl_pa0001-stat2 = w_pa00-stat2.
          wl_pa0001-cname = w_pa02-cname.
          wl_pa0001-gbdat = w_pa02-gbdat.
          APPEND wl_pa0001 TO tl_pa0001.
        ENDIF.
      ENDIF.
    ENDLOOP.

*    Busca lista de email de todos os funcionários:
    SELECT pernr, usrid_long
        FROM pa0105
        INTO TABLE @DATA(lit_0105)
        WHERE subty = 'MAIL'
          AND endda >= @vl_data
          AND begda <= @vl_data
     ORDER BY pernr ASCENDING.

    SELECT pernr, endda, dat01
      FROM pa0041
      INTO TABLE @DATA(lit_pa0041)
       FOR ALL ENTRIES IN @t_pa01
     WHERE pernr EQ @t_pa01-pernr AND
           endda GE @vl_data AND
           begda LE @vl_data.
    SORT lit_pa0041[] BY pernr ASCENDING.

    SELECT pernr, endda, schkz
      FROM pa0007
      INTO TABLE @DATA(lit_pa0007)
       FOR ALL ENTRIES IN @t_pa01
     WHERE pernr EQ @t_pa01-pernr AND
           endda GE @vl_data AND
           begda LE @vl_data.
    SORT lit_pa0007[] BY pernr ASCENDING.

    SELECT pernr, endda, awart
      FROM pa2001
      INTO TABLE @DATA(lit_2001) "l_it_2001
      FOR ALL ENTRIES IN @t_pa01
     WHERE pernr EQ @t_pa01-pernr
       AND endda GE @vl_data
       AND begda LE @vl_data.
    SORT lit_2001[] BY pernr ASCENDING.

    DATA(rg_orgeh) = VALUE ty_rg_orgeh( FOR lwa_0001 IN t_pa01[] (
        sign = 'I' option = 'EQ' low = lwa_0001-orgeh )
    ).
    SORT rg_orgeh[] BY low ASCENDING.
    DELETE ADJACENT DUPLICATES FROM rg_orgeh[] COMPARING low.

    DATA(rg_plans) = VALUE ty_rg_plans( FOR lwa_0001 IN t_pa01[] (
        sign = 'I' option = 'EQ' low = lwa_0001-plans )
    ).
    SORT rg_plans[] BY low ASCENDING.
    DELETE ADJACENT DUPLICATES FROM rg_plans[] COMPARING low.

*   Descrição das uniorgs:
    SELECT objid, stext FROM hrp1000
      INTO TABLE @DATA(lit_uniorgs)
       WHERE plvar = '01'
         AND otype = 'O'
         AND objid IN @rg_orgeh[] " wl_pa0001-orgeh
         AND langu =  @sy-langu
         AND endda >= @sy-datum
         ORDER BY objid ASCENDING.

*   Descrição das Posições:
    SELECT objid, stext FROM hrp1000
       INTO TABLE @DATA(lit_posicoes)
       WHERE plvar = '01'
         AND otype = 'S'
         AND objid IN @rg_plans[] " wl_pa0001-orgeh
         AND langu = @sy-langu
         AND endda >= @sy-datum "Estava pegando cargo incorreto 70037708
       ORDER BY objid ASCENDING.

  ENDIF.

  LOOP AT tl_pa0001 INTO wl_pa0001.

    CLEAR: wa_func_list, t_saida_superior[], t_saida_superior.

    v_pernr = wl_pa0001-pernr.

    IF vl_data IS NOT INITIAL.
      CALL FUNCTION 'ZHCMF_RET_SUPERIOR_9002'
        EXPORTING
          pernr          = wl_pa0001-pernr
          tp_gestor      = 'I'
        TABLES
          t_saida        = t_saida_superior
        CHANGING
          c_data_posicao = vl_data.
    ELSE.
      CALL FUNCTION 'ZHCMF_RET_SUPERIOR_9002'
        EXPORTING
          pernr     = wl_pa0001-pernr
          tp_gestor = 'I'
        TABLES
          t_saida   = t_saida_superior.
    ENDIF.

    IF ( t_saida_superior[] IS NOT INITIAL ).
      SORT t_saida_superior[] BY tp_gest ASCENDING.

      READ TABLE t_saida_superior[] ASSIGNING FIELD-SYMBOL(<lfs_superior>)
        WITH KEY tp_gest = 'I' BINARY SEARCH.
      IF sy-subrc IS INITIAL.

        wa_func_list-sup_pernr  = <lfs_superior>-pernr.
        wa_func_list-sup_cname  = <lfs_superior>-cname.
        wa_func_list-sup_cpf_nr = <lfs_superior>-cpf_nr.

        READ TABLE lit_0105[] ASSIGNING FIELD-SYMBOL(<lfs_0105_sup>)
            WITH KEY pernr = wa_func_list-sup_pernr BINARY SEARCH.
        IF ( sy-subrc = 0 ).
          wa_func_list-sup_email  = <lfs_0105_sup>-usrid_long.
        ENDIF.

      ENDIF.

    ENDIF.

    wa_func_list-cname = wl_pa0001-cname.
    wa_func_list-abkrs = wl_pa0001-abkrs .

    READ TABLE lit_2001[] ASSIGNING FIELD-SYMBOL(<lfs_2001>)
        WITH KEY pernr = v_pernr BINARY SEARCH.
    IF sy-subrc IS INITIAL.
      wa_func_list-awart = <lfs_2001>-awart.
    ENDIF.

    CLEAR: w_t549t.

    READ TABLE t_t549t[] ASSIGNING FIELD-SYMBOL(<lfs_t549t>)
        WITH KEY abkrs = wl_pa0001-abkrs BINARY SEARCH.
    IF ( sy-subrc = 0 ).
      wa_func_list-areafpg = <lfs_t549t>-atext.
    ENDIF.

    READ TABLE lit_0465[] ASSIGNING FIELD-SYMBOL(<lfs_0465>)
        WITH KEY pernr = v_pernr subty = '0001' BINARY SEARCH.
    IF ( sy-subrc = 0 ).
      wa_func_list-cpf_nr = <lfs_0465>-cpf_nr.
    ENDIF.

    READ TABLE lit_0465[] ASSIGNING <lfs_0465>
        WITH KEY pernr = v_pernr subty = '0002' BINARY SEARCH.
    IF ( sy-subrc = 0 ).
      wa_func_list-ident_nr    = <lfs_0465>-ident_nr.
      wa_func_list-ident_org   = <lfs_0465>-ident_org.
    ENDIF.

    READ TABLE lit_0465[] ASSIGNING <lfs_0465>
       WITH KEY pernr = v_pernr subty = '0003' BINARY SEARCH.
    IF ( sy-subrc = 0 ).
      wa_func_list-ctps_nr     = <lfs_0465>-ctps_nr.
      wa_func_list-ctps_serie  = <lfs_0465>-ctps_serie.
    ENDIF.

    READ TABLE lit_0465[] ASSIGNING <lfs_0465>
        WITH KEY pernr = v_pernr subty = '0006' BINARY SEARCH.
    IF ( sy-subrc = 0 ).
      wa_func_list-pis_nr      = <lfs_0465>-pis_nr.
    ENDIF.


    READ TABLE lit_0105[] ASSIGNING FIELD-SYMBOL(<lfs_0105>)
            WITH KEY pernr = v_pernr BINARY SEARCH.
    IF ( sy-subrc = 0 ).
      wa_func_list-email = <lfs_0105>-usrid_long.
    ENDIF.

    wa_func_list-pernr      = wl_pa0001-pernr.
    wa_func_list-persk      = wl_pa0001-persk.

    IF wl_pa0001-bukrs = '0050'.

      READ TABLE t_pa00 INTO w_pa00 WITH KEY pernr = v_pernr.

      IF sy-subrc = 0.

        wa_func_list-endda      = w_pa00-endda.
        wa_func_list-begda      = w_pa00-begda.

      ENDIF.

    ELSE.

      wa_func_list-endda      = wl_pa0001-endda.
      wa_func_list-begda      = wl_pa0001-begda.

    ENDIF.

    wa_func_list-gbdat      = wl_pa0001-gbdat.
    wa_func_list-stat2      = wl_pa0001-stat2.
    wa_func_list-kokrs      = wl_pa0001-kokrs.
    wa_func_list-kostl      = wl_pa0001-kostl.


    CLEAR: w_t503t.

    READ TABLE t_t503t INTO w_t503t WITH KEY persk = wl_pa0001-persk.
    wa_func_list-ptext = w_t503t-ptext.
    wa_func_list-bukrs = wl_pa0001-bukrs.

    CALL FUNCTION 'HRWPC_RFC_BUKRS_TEXT_GET'
      EXPORTING
        bukrs      = wl_pa0001-bukrs
        langu      = sy-langu
      IMPORTING
        bukrs_text = wa_func_list-empresa.


*    DADOS DA EMPRESA
*    Endereco Empresa (Rua , Numero, Complemento, Bairro, Cidade, Estado, CEP, Telefone)
    CALL FUNCTION 'HR_BR_GET_FILIAL_PER_AREA'
      EXPORTING
        p_werks        = wl_pa0001-werks
        p_btrtl        = wl_pa0001-btrtl
      IMPORTING
        branch         = wa_branch
      EXCEPTIONS
        no_link_areas  = 1
        no_group_found = 2
        OTHERS         = 3.
    IF sy-subrc IS INITIAL.
      "Endereço e CGC
      CALL FUNCTION 'HR_BR_LER_FILIAL_GERAL'
        EXPORTING
          company_code      = wl_pa0001-bukrs
          branch            = wa_branch
          date              = '99991231'
        IMPORTING
          cgc               = cgc_nr
          comp_name         = name
          comp_addr         = it_addr
        EXCEPTIONS
          branch_not_found  = 1
          address_not_found = 2
          company_not_found = 3
          OTHERS            = 4.

      CALL FUNCTION 'CONVERSION_EXIT_CGCBR_OUTPUT'
        EXPORTING
          input  = cgc_nr
        IMPORTING
          output = wa_func_list-cgc_number.

      CONCATENATE it_addr-street ', ' it_addr-house_num1 '  ' it_addr-city2 '  '
    it_addr-city1 ' - ' it_addr-region ' ' it_addr-post_code1 ' ' it_addr-tel_number INTO   wa_func_list-end_empresa SEPARATED BY space.

    ENDIF.

    wa_func_list-werks = wl_pa0001-werks.

    CALL FUNCTION 'HRWPC_RFC_WERKS_TEXT_GET'
      EXPORTING
        werks      = wl_pa0001-werks
      IMPORTING
        werks_text = v_werks_text.

    wa_func_list-arearh = v_werks_text.



    SELECT SINGLE text1 FROM t529u
      INTO wa_func_list-situacao
           WHERE sprsl = sy-langu
             AND statn = '2'
             AND statv = wl_pa0001-stat2.

    wa_func_list-orgeh = wl_pa0001-orgeh.
    wa_func_list-plans = wl_pa0001-plans.

*   Descrição das uniorgs:
    READ TABLE lit_uniorgs[] ASSIGNING FIELD-SYMBOL(<lfs_uniorg>)
      WITH KEY objid = wl_pa0001-orgeh BINARY SEARCH.
    IF ( sy-subrc = 0 ).
      wa_func_list-uniorg = <lfs_uniorg>-stext.
    ENDIF.

*   Descrição das Posições:
    READ TABLE lit_posicoes[] ASSIGNING FIELD-SYMBOL(<lfs_posicoes>)
          WITH KEY objid = wl_pa0001-plans BINARY SEARCH.
    IF ( sy-subrc = 0 ).
      wa_func_list-posicao = <lfs_posicoes>-stext.
    ENDIF.


    SELECT SINGLE ktext  FROM cskt
     INTO wa_func_list-ccusto
      WHERE kokrs = wl_pa0001-kokrs
        AND kostl = wl_pa0001-kostl
        AND spras = sy-langu.

    READ TABLE lit_pa0041[] ASSIGNING FIELD-SYMBOL(<lfs_0041>)
        WITH KEY pernr = v_pernr BINARY SEARCH.
    IF ( sy-subrc = 0 ).
      wa_func_list-dat01 = <lfs_0041>-dat01. "ADMISSAO
    ENDIF.


    "DATA DE DEMISSÃO
    CALL FUNCTION 'RP_GET_FIRE_DATE'
      EXPORTING
        persnr   = wl_pa0001-pernr
        "STATUS2  = '0'
      IMPORTING
        firedate = wa_func_list-fdate.

    READ TABLE lit_pa0007[] ASSIGNING FIELD-SYMBOL(<lfs_0007>)
        WITH KEY pernr = wl_pa0001-pernr BINARY SEARCH.
    IF ( sy-subrc = 0 ).
      wa_func_list-schkz = <lfs_0007>-schkz.
    ENDIF.

    SELECT SINGLE rtext FROM t508s
       INTO wa_func_list-horario
            WHERE zeity = 1
              AND mofid = '2A'
              AND sprsl = sy-langu
              AND mosid = 37
              AND schkz = wa_func_list-schkz. "l_0007-schkz.

    APPEND wa_func_list TO t_saida.

    CLEAR: l_it_0105, l_it_0465, l_it_0041, l_it_0007, l_0105, l_0465, w_t503t,
           l_0041, l_0007, l_2001, v_werks_text, wa_result_struc, wa_func_list, wl_pa0001.

  ENDLOOP.

ENDFUNCTION.
