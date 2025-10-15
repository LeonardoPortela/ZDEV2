*&---------------------------------------------------------------------*
*& Report  Z_GRAVA_ZIB_ZAD
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  z_grava_zib_zadii.

DATA: BEGIN OF it_msg OCCURS 0.
        INCLUDE STRUCTURE bdcmsgcoll.
DATA: END OF it_msg.
DATA: wl_mode(1).

TYPES:
  BEGIN OF ty_ekpo,
    ebeln TYPE ekpo-ebeln,
    ebelp TYPE ekpo-ebelp,
    werks TYPE ekpo-werks,
  END OF ty_ekpo,

  BEGIN OF ty_zfit0046,
    nro_sol          TYPE zfit0046-nro_sol,
    ebeln            TYPE zfit0046-ebeln,
    ebelp            TYPE zfit0046-ebelp,
    anln1            TYPE zfit0046-anln1,
    anln2            TYPE zfit0046-anln2,
    vlr_adiantamento TYPE zfit0046-vlr_adiantamento,
    del(1),
  END OF ty_zfit0046,

  BEGIN OF ty_bsik,
    bukrs TYPE bsik-bukrs,
    lifnr TYPE bsik-lifnr,
    belnr TYPE bsik-belnr,
    dmbtr TYPE bsik-dmbtr,
    dmbe2 TYPE bsik-dmbe2,
    budat TYPE bsik-budat,
    buzei TYPE bsik-buzei,
    gsber TYPE bsik-gsber,
    shkzg TYPE bsik-shkzg,
    umskz TYPE bsik-umskz,
  END OF ty_bsik.


DATA: ti_bdcdata TYPE STANDARD TABLE OF bdcdata ,   "Guarda o mapeamento
      t_messtab  TYPE TABLE OF bdcmsgcoll,
*<--- Inicio Alteração - MG-5592 - YA
      g_gjahr    TYPE gjahr.
*<--- Fim Alteração - MG-5592 - YA


*----------------------------------------------------------------------*
* WORK AREA
*----------------------------------------------------------------------*
DATA:
  wa_cont          TYPE REF TO cl_gui_custom_container,
  wa_alv           TYPE REF TO cl_gui_alv_grid,
  wa_bdcdata       LIKE LINE OF ti_bdcdata,
  tabix            TYPE sy-tabix,
  tabix2           TYPE sy-tabix,
  vnum(10)         TYPE c,
  vseq(10)         TYPE p,
  wl_erro(1),
  wg_documento(10),
  p_alv            TYPE zfit0045,
  wa_zfit0036_ins  TYPE zfit0036,
  wa_bsik          TYPE ty_bsik,
  it_bsik_aux      TYPE TABLE OF ty_bsik.

SELECTION-SCREEN: BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
  PARAMETERS: p_sol  TYPE zfit0045-nro_sol.

SELECTION-SCREEN: END OF BLOCK b1.

START-OF-SELECTION.


  PERFORM f_shdb USING p_sol CHANGING wl_erro.
*&---------------------------------------------------------------------*
*&      Form  F_SHDB
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_WG_EXTERNO  text
*      <--P_WL_ERRO  text
*----------------------------------------------------------------------*
FORM f_shdb  USING  p_nro_sol  CHANGING p_erro.
  DATA: vdata(10),
        vdata_v(10),
        wl_vlr(16),
        wl_tax(16),
        v_xblnr        TYPE bkpf-xblnr,
        v_nro_sol(10),
        vlines         TYPE sy-tabix,
        it_zfit0046    TYPE TABLE OF ty_zfit0046,
        it_zfit0046_im TYPE TABLE OF ty_zfit0046,
        wa_zfit0046    TYPE ty_zfit0046,
        it_ekpo        TYPE TABLE OF ty_ekpo,
        wa_ekpo        TYPE ty_ekpo,
        vbsart         TYPE ekko-bsart,
        vgsber         TYPE t134g-gsber,
        lva_nro_sol    TYPE zfit0045-nro_sol,
        zebelp         TYPE zmmt0037-ebelp.

  DATA: xv_jobnm TYPE btcjob.
  DATA: xv_stepc TYPE btcstepcnt.

  IF sy-batch IS NOT  INITIAL.
    CALL FUNCTION 'GET_JOB_RUNTIME_INFO'
      IMPORTING
        jobname         = xv_jobnm
        stepcount       = xv_stepc
      EXCEPTIONS
        no_runtime_info = 1
        OTHERS          = 2.

    IF xv_jobnm+0(17) EQ 'Z_GRAVA_ZIB_ZADII'.
      SPLIT xv_jobnm AT '|' INTO DATA(lv_desc)
                              DATA(lv_nro_sol).
      IF lv_nro_sol IS NOT INITIAL.
        p_nro_sol = lv_nro_sol.
      ENDIF.
    ENDIF.
  ENDIF.

  CHECK          p_nro_sol IS NOT INITIAL.

  SELECT SINGLE * FROM zfit0045 INTO p_alv WHERE nro_sol EQ p_nro_sol.
  CHECK sy-subrc EQ 0. "Check se solicitação existe...

  "Bloquear registro para prosseguir por até 3 minutos.. se não conseguir, prosseguir mesmo assim, pois já irá ter evitado a duplicidade na geração dos docs. contabeis...
  lva_nro_sol  = p_nro_sol.

  DO 60 TIMES.

    "Bloqueia solicitação de adiantamento
    CALL FUNCTION 'ENQUEUE_EZFIT0045'
      EXPORTING
        nro_sol        = lva_nro_sol
      EXCEPTIONS
        foreign_lock   = 1
        system_failure = 2
        OTHERS         = 3.

    IF sy-subrc EQ 0.
      EXIT.
    ENDIF.

    WAIT UP TO 3 SECONDS.

  ENDDO.

  SELECT SINGLE *
    FROM zfit0045
    INTO p_alv
    WHERE nro_sol EQ p_nro_sol.

  CHECK sy-subrc EQ 0.

  "Verificar se documento contabil já foi gerado, e caso sim, não deixar prosseguir
  IF ( p_alv-belnr IS NOT INITIAL ).
    SELECT SINGLE *
      FROM bkpf INTO @DATA(lwa_bkpf_chk)
     WHERE bukrs EQ @p_alv-bukrs
       AND belnr EQ @p_alv-belnr.

    IF ( sy-subrc EQ 0 ) AND ( lwa_bkpf_chk-stblg IS INITIAL ).
      "Desbloqueia solicitação de adiantamento
      CALL FUNCTION 'DEQUEUE_EZFIT0045'
        EXPORTING
          nro_sol = lva_nro_sol.





      p_erro = abap_true.
      EXIT.
    ENDIF.

*<--- Inicio Alteração - MG-5592 - YA
    IF lwa_bkpf_chk-gjahr IS NOT INITIAL.
      g_gjahr = lwa_bkpf_chk-gjahr.
    ENDIF.
*<--- Fim Alteração - MG-5592 - YA
  ENDIF.



  SELECT  nro_sol ebeln  ebelp anln1 anln2 vlr_adiantamento
  FROM zfit0046
  INTO TABLE it_zfit0046
  WHERE nro_sol EQ p_nro_sol.

  CHECK it_zfit0046[] IS NOT INITIAL.

  " agrupar imobilizados dentro do pedido independente da linha (EBELP)
  SELECT  nro_sol, ebeln,  anln1, anln2,
    SUM( vlr_adiantamento ) AS vlr_adiantamento
    FROM zfit0046
    INTO TABLE @DATA(it_zfit0046_copy)
    WHERE nro_sol EQ @p_nro_sol
    AND   anln1   NE ' '
  GROUP BY nro_sol, ebeln,  anln1, anln2
  HAVING COUNT(*) > 1 .

  " somente para imobilizados repetidos
  IF it_zfit0046_copy[] IS NOT INITIAL.
    it_zfit0046_im[] = it_zfit0046[].

    SORT it_zfit0046_im BY  nro_sol ebeln anln1 anln2.
    DELETE ADJACENT DUPLICATES FROM it_zfit0046_im COMPARING nro_sol ebeln  anln1 anln2.
    LOOP AT it_zfit0046 INTO wa_zfit0046 WHERE nro_sol = p_alv-nro_sol
                                         AND   ebeln   = p_alv-ebeln.
      DATA(tabix) = sy-tabix.
      READ TABLE it_zfit0046_im INTO DATA(wa_zfit0046_im) WITH KEY anln1 =  wa_zfit0046-anln1.
      IF sy-subrc = 0.
        IF wa_zfit0046-ebelp NE wa_zfit0046_im-ebelp.
          wa_zfit0046-del = 'X'.
          MODIFY it_zfit0046 FROM wa_zfit0046 INDEX tabix TRANSPORTING del.
        ENDIF.
      ENDIF.
    ENDLOOP.
    DELETE it_zfit0046 WHERE del = 'X'.
    LOOP AT it_zfit0046 INTO wa_zfit0046 WHERE nro_sol = p_alv-nro_sol
                                         AND   ebeln   = p_alv-ebeln.
      DATA(tabix2) = sy-tabix.
      READ TABLE it_zfit0046_copy INTO DATA(wa_zfit0046_copy) WITH KEY anln1 =  wa_zfit0046-anln1.
      IF sy-subrc = 0.
        wa_zfit0046-vlr_adiantamento = wa_zfit0046_copy-vlr_adiantamento.
        MODIFY it_zfit0046 FROM wa_zfit0046 INDEX tabix2 TRANSPORTING vlr_adiantamento.
      ENDIF.
    ENDLOOP.
  ENDIF.

  SELECT SINGLE bsart
    FROM ekko
    INTO vbsart
    WHERE ebeln = p_alv-ebeln.

  SELECT ebeln ebelp werks
    FROM ekpo
    INTO TABLE it_ekpo
    FOR ALL ENTRIES IN it_zfit0046
    WHERE ebeln EQ it_zfit0046-ebeln
    AND   ebelp EQ it_zfit0046-ebelp.

  SORT it_ekpo BY ebeln ebelp.
  "
  vlines = 0.
  LOOP AT it_zfit0046 INTO wa_zfit0046 WHERE nro_sol = p_alv-nro_sol
                                       AND   ebeln   = p_alv-ebeln.
    ADD 1 TO vlines.
  ENDLOOP.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = p_alv-nro_sol
    IMPORTING
      output = v_nro_sol.

  CONCATENATE 'SOL.' v_nro_sol INTO v_xblnr.
  REFRESH ti_bdcdata.
  CONCATENATE  sy-datum+6(2) sy-datum+4(2) sy-datum+0(4) INTO vdata SEPARATED BY '.'.
  CONCATENATE  p_alv-dt_pgto+6(2) p_alv-dt_pgto+4(2) p_alv-dt_pgto(4) INTO vdata_v SEPARATED BY '.'.
  tabix2 = 0.

  PERFORM f_bdc_data USING:
    'SAPMF05A'  '0112'  'X'  ''                 ' ',
    ''          ''      ''   'BDC_OKCODE'        '/00',
    ''          ''      ''   'BKPF-BLDAT'       vdata,
    ''          ''      ''   'BKPF-BLART'       'KA',
    ''          ''      ''   'BKPF-BUKRS'       p_alv-bukrs,
    ''          ''      ''   'BKPF-BUDAT'       vdata,
    ''          ''      ''   'BKPF-MONAT'       p_alv-dt_pgto+4(2),
    ''          ''      ''   'BKPF-WAERS'       p_alv-moeda_pgto,
    ''          ''      ''   'BKPF-XBLNR'       v_xblnr,
    ''          ''      ''   'RF05A-NEWKO'      p_alv-lifnr.
  IF 'ZDEF_ZEFI_ZFTE_ZSEM' CS vbsart.
    PERFORM f_bdc_data USING:
      ''          ''      ''   'RF05A-ZUMSK'      'M'.
  ELSE.
    PERFORM f_bdc_data USING:
    ''          ''      ''   'RF05A-ZUMSK'      'A'.
  ENDIF.

  IF p_alv-taxa GT 0 AND p_alv-moeda_pgto = 'USD'.
    WRITE: p_alv-taxa                TO wl_tax.
    TRANSLATE wl_tax USING '. ,'.
    CONDENSE wl_tax NO-GAPS.
    PERFORM f_bdc_data USING:
           ''          ''      ''   'BKPF-KURSF'        wl_tax.
  ENDIF.
  LOOP AT it_zfit0046 INTO wa_zfit0046 WHERE nro_sol = p_alv-nro_sol
                                       AND   ebeln   = p_alv-ebeln.
    ADD 1 TO tabix2.
    WRITE: wa_zfit0046-vlr_adiantamento                TO wl_vlr.
    TRANSLATE wl_vlr USING '. ,'.
    CONDENSE wl_vlr NO-GAPS.
    CLEAR: wa_ekpo, vgsber.
    READ TABLE it_ekpo INTO wa_ekpo WITH KEY ebeln = wa_zfit0046-ebeln
                                             ebelp = wa_zfit0046-ebelp BINARY SEARCH.
    IF sy-subrc NE 0.
      READ TABLE it_ekpo INTO wa_ekpo WITH KEY ebeln = wa_zfit0046-ebeln.
      IF sy-subrc = 0.
        vgsber = wa_ekpo-werks.
      ENDIF.
    ELSE.
      vgsber = wa_ekpo-werks.
    ENDIF.

    IF wa_ekpo-werks IS NOT INITIAL.
      SELECT SINGLE *
           FROM t134g
           INTO @DATA(_t134g)
           WHERE werks = @wa_ekpo-werks.
      IF sy-subrc = 0.
        vgsber = _t134g-gsber.
      ENDIF.
    ENDIF.

    PERFORM f_bdc_data USING:
            'SAPMF05A'  '0304'  'X'  ''                  ' ',
            ''          ''      ''   'BDC_OKCODE'        '=ZK'.

*** PBI - 60949 - CS2020001160 ZMM0149 - Inicio - CSB
    IF wa_zfit0046-ebeln IS NOT INITIAL.
      CLEAR: zebelp.
      zebelp = wa_zfit0046-ebeln.
      zebelp = |{ zebelp ALPHA = IN }|.
      SELECT SINGLE *
           FROM zmmt0037
           INTO @DATA(_t_zmmt0037)
           WHERE ebelp = @zebelp.
      IF sy-subrc = 0.
        p_alv-identificador = _t_zmmt0037-nro_sol_cp.
      ELSE.
        SELECT SINGLE *
          FROM zmmt0035
          INTO @DATA(_t_zmmt0035)
          WHERE ebeln = @wa_zfit0046-ebeln.
        IF sy-subrc = 0.
          p_alv-identificador = _t_zmmt0035-nro_sol_cp.
        ENDIF.
      ENDIF.
    ENDIF.
*** PBI - 60949 - CS2020001160 ZMM0149 - Fim

    IF p_alv-hbkid_e IS NOT INITIAL.
      CLEAR p_alv-hbkid.
      p_alv-zlsch  = 'V'.
    ENDIF.













    PERFORM f_bdc_data USING:
    ''          ''      ''   'BSEG-WRBTR'        wl_vlr,
    ''          ''      ''   'BSEG-GSBER'        vgsber,
    ''          ''      ''   'BSEG-ZFBDT'        vdata_v,
    ''          ''      ''   'BSEG-ZLSCH'        p_alv-zlsch,
    ''          ''      ''   'BSEG-KIDNO'        p_alv-identificador,
    ''          ''      ''   'BSEG-ANLN1'        wa_zfit0046-anln1,
    ''          ''      ''   'BSEG-ANLN2'        wa_zfit0046-anln2,
    ''          ''      ''   'BSEG-EBELN'        wa_zfit0046-ebeln,
    ''          ''      ''   'BSEG-EBELP'        wa_zfit0046-ebelp,
    ''          ''      ''   'BSEG-SGTXT'        p_alv-sgtxt.

    IF tabix2 = vlines.
      PERFORM f_bdc_data USING:
      'SAPMF05A'  '0332'  'X'  ''                 ' ',
      ''          ''      ''   'BDC_OKCODE'        '/00'.
    ELSE.
      PERFORM f_bdc_data USING:
      'SAPMF05A'  '0332'  'X'  ''                 ' ',
      ''          ''      ''   'BDC_OKCODE'        '=NP'.
    ENDIF.






    PERFORM f_bdc_data USING:
    ''          ''      ''   'BSEG-BVTYP'        p_alv-bvtyp,
    ''          ''      ''   'BSEG-HBKID'        p_alv-hbkid.
  ENDLOOP.

  PERFORM f_bdc_data USING:
    'SAPMF05A'  '0332'  'X'  ''                 ' ',
    ''          ''      ''   'BDC_OKCODE'        '=BU'.


  CLEAR p_erro.
  "
  PERFORM zf_call_transaction USING 'F-47' CHANGING p_erro.
  IF p_erro = 'X'.
    ROLLBACK WORK.
  ELSE.

    UPDATE zfit0045 SET belnr = wg_documento
                    status = 'A'
                    WHERE nro_sol =  p_nro_sol.
    COMMIT WORK.


    "Executar workflow SFC.
    SELECT SINGLE * FROM zfit0045 INTO @DATA(l_zfit0045) WHERE nro_sol EQ @p_nro_sol.
    IF l_zfit0045-orig_pgt EQ 'E'  AND l_zfit0045-form_pgt = 'C'.









      "Criando Workflow tipo SFC no SE.
      IF wg_documento IS NOT INITIAL.
        l_zfit0045-belnr = wg_documento.
        sy-uname = l_zfit0045-usnam.

        zcl_int_se=>create_workflow_softexpert_sfc(
              EXPORTING
            solicitacao = l_zfit0045
              IMPORTING
                e_recordid = DATA(e_recordid)
                e_msg      = DATA(e_msg)
        ).

        IF e_recordid IS NOT INITIAL.
          UPDATE zfit0045 SET nro_sm_se = e_recordid WHERE nro_sol EQ p_nro_sol.
          COMMIT WORK.
        ENDIF.
      ENDIF.
    ELSEIF l_zfit0045-orig_pgt EQ 'E'  AND l_zfit0045-form_pgt = 'T'.
      IF p_alv-hbkid_e IS NOT INITIAL.
        WAIT UP TO 5 SECONDS.
        PERFORM f_gera_adto17.

      ENDIF.
    ENDIF.
  ENDIF.














  "Desbloqueia solicitação de adiantamento
  CALL FUNCTION 'DEQUEUE_EZFIT0045'
    EXPORTING
      nro_sol = lva_nro_sol.

ENDFORM.                    " F_SHDB


*&---------------------------------------------------------------------*
*&      Form  ZF_CALL_TRANSACTION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_2296   text
*      <--P_P_ERRO  text
*----------------------------------------------------------------------*
FORM zf_call_transaction  USING p_trans CHANGING p_erro.
  CONSTANTS: c_msgid LIKE it_msg-msgid VALUE 'F5',
             c_msgnr LIKE it_msg-msgnr VALUE '312',
             c_msgne LIKE it_msg-msgnr VALUE '539'.

  REFRESH it_msg.

  wl_mode = 'E'.
  CALL TRANSACTION p_trans USING ti_bdcdata
        MODE wl_mode
        MESSAGES INTO it_msg.

  READ TABLE it_msg WITH KEY msgtyp = 'A'.
  IF sy-subrc = 0.
    p_erro = 'X'.
  ELSE.
    READ TABLE it_msg WITH KEY msgtyp = 'E'.
    IF sy-subrc = 0.
      p_erro = 'X'.
    ENDIF.
  ENDIF.

  CLEAR wg_documento.

  READ TABLE it_msg WITH KEY msgid = c_msgid
                             msgnr = c_msgnr
                             msgtyp = 'S'.

  IF sy-subrc = 0.
    MOVE it_msg-msgv1 TO wg_documento.
  ENDIF.

  IF  wg_documento IS INITIAL.
    p_erro = 'X'.
  ELSE.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = wg_documento
      IMPORTING
        output = wg_documento.
  ENDIF.



ENDFORM.                    " ZF_CALL_TRANSACTION

*&---------------------------------------------------------------------*
*&      Form  F_BDC_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_PROGRAM  text
*      -->P_DYNPRO   text
*      -->P_START    text
*      -->P_FNAM     text
*      -->P_FVAL     text
*----------------------------------------------------------------------*
FORM f_bdc_data  USING p_program p_dynpro p_start p_fnam p_fval.
* Este form recebe cada conteúdo passado em ordem para os parâmetros de
* entrada e abaixo preenche a wa_bdcdata que por sua vez carrega a ti_bdcdata.
  CLEAR wa_bdcdata.
  wa_bdcdata-program   = p_program.
  wa_bdcdata-dynpro    = p_dynpro.
  wa_bdcdata-dynbegin  = p_start.
  wa_bdcdata-fnam      = p_fnam.
  wa_bdcdata-fval      = p_fval.
  APPEND wa_bdcdata TO ti_bdcdata.

ENDFORM.                    " F_BDC_DATA

FORM f_gera_adto17.
  DATA: vseq(10)      TYPE p,
        vlote_ad(10)  TYPE c,
        vnum2(11)     TYPE c,
        v_nro_sol(10),
        vtabkey       TYPE tiban-tabkey,
        vbvtyp        TYPE lfbk-bvtyp,
        vbvtyp2       TYPE lfbk-bvtyp.


  p_alv-belnr = wg_documento.
  " Gera numero do lote
  CALL FUNCTION 'NUMBER_GET_NEXT'
    EXPORTING
      nr_range_nr = '01'
      object      = 'ZID_INV'
    IMPORTING
      number      = vseq.
  vlote_ad = vseq .
  "
  " Sequencia "AC"
  CALL FUNCTION 'NUMBER_GET_NEXT'
    EXPORTING
      nr_range_nr = '01'
      object      = 'ZID_FI17'
    IMPORTING
      number      = vseq.
  vnum2 = vseq .
  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = vnum2
    IMPORTING
      output = vnum2.
  "
  SELECT SINGLE *
  FROM zfit0043
  INTO @DATA(wa_zfit0043)
  WHERE tp_operacao = '08'
    AND spras = @sy-langu. "PGTO DE PEDIDOS/ZGL

  CLEAR wa_zfit0036_ins.
  CONCATENATE 'AC' vnum2 p_alv-lifnr INTO wa_zfit0036_ins-obj_key.
  "
  wa_zfit0036_ins-bukrs          = p_alv-bukrs.
  wa_zfit0036_ins-lote           = vlote_ad.
  wa_zfit0036_ins-invoice        = wa_zfit0043-ds_operacao.
  wa_zfit0036_ins-dt_pgto        = p_alv-dt_pgto.
  wa_zfit0036_ins-moeda_pgto     = p_alv-moeda_pgto.
*  WA_ZFIT0036_INS-VLR_PGTO       = 0.
  SELECT SUM( wrbtr )                  "#EC CI_DB_OPERATION_OK[2431747]
   INTO wa_zfit0036_ins-vlr_pgto
   FROM bseg
   WHERE bukrs = p_alv-bukrs
   AND   belnr = p_alv-belnr.

  wa_zfit0036_ins-hbkid          = p_alv-hbkid_e.  " Na ZFI0017 - indica o banco novamente?
  wa_zfit0036_ins-status         = 'L'. "Liberadas automaticamente pedido/ZGL
  wa_zfit0036_ins-forma_pg       = 'P'.
  wa_zfit0036_ins-motivo         = '08'. " payments of imports

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = p_alv-nro_sol
    IMPORTING
      output = v_nro_sol.

  CONCATENATE 'SOL.' v_nro_sol INTO wa_zfit0036_ins-referencia.

  wa_zfit0036_ins-observacao = p_alv-sgtxt.
  wa_zfit0036_ins-operacao   = '08'.
  wa_zfit0036_ins-lifnr      = p_alv-lifnr.
  wa_zfit0036_ins-usuario    = sy-uname.
  wa_zfit0036_ins-data_atual = sy-datum.
  wa_zfit0036_ins-hora_atual = sy-uzeit.

  SELECT SINGLE *
     FROM lfbk
      INTO @DATA(wl_lfbk)
      WHERE lifnr = @p_alv-lifnr.

  IF p_alv-bvtyp IS NOT INITIAL.
    SELECT SINGLE *
         FROM lfbk
         INTO wl_lfbk
         WHERE lifnr = p_alv-lifnr
         AND   bvtyp = p_alv-bvtyp.
    IF sy-subrc NE 0.
      SELECT SINGLE *
          FROM lfbk
           INTO wl_lfbk
           WHERE lifnr = p_alv-lifnr.
    ENDIF.
  ENDIF.

  SELECT SINGLE *
    FROM bnka
    INTO @DATA(wl_bnka)
    WHERE banks	=	@wl_lfbk-banks
    AND   bankl	=	@wl_lfbk-bankl.

  SELECT  *
    FROM tiban
    INTO TABLE @DATA(tl_tiban)
    WHERE banks   = @wl_lfbk-banks
    AND   bankl   = @wl_lfbk-bankl
    AND   bankn   = @wl_lfbk-bankn
    AND   tabname IN ('LFBK', 'BUT0BK')
    ORDER BY erdat DESCENDING.
  IF sy-subrc = 0.
    READ TABLE tl_tiban INTO DATA(wl_tiban) INDEX 1.
    wa_zfit0036_ins-iban_1  = wl_tiban-iban.
  ENDIF.

  wa_zfit0036_ins-bvtyp   = wl_lfbk-bvtyp.
  wa_zfit0036_ins-swift_1 = wl_bnka-swift.
  wa_zfit0036_ins-banks_1 = wl_lfbk-banks.
  wa_zfit0036_ins-bankl_1 = wl_lfbk-bankl.
  wa_zfit0036_ins-banka_1 = wl_bnka-banka.
  wa_zfit0036_ins-bankn_1 = wl_lfbk-bankn.
  "Não compensa o MEMO, sera feito nsa ZFI0017
  wa_zfit0036_ins-belnr_adt_c = wg_documento.
  vbvtyp2 = p_alv-bvtyp.
  CONCATENATE vbvtyp2+0(3) '2' INTO vbvtyp.

  SELECT SINGLE *
   FROM lfbk
   INTO wl_lfbk
   WHERE lifnr = p_alv-lifnr
   AND bvtyp  = vbvtyp.

  IF sy-subrc = 0.
    MOVE p_alv-lifnr TO vtabkey.
    SELECT SINGLE *
       FROM tiban
       INTO wl_tiban
       WHERE banks   =  wl_lfbk-banks
       AND   bankl   =  wl_lfbk-bankl
       AND   bankn   =  wl_lfbk-bankn
       AND   tabkey  =  vtabkey
       AND   tabname IN ('LFBK', 'BUT0BK').
    IF sy-subrc NE 0.
      CLEAR wl_tiban.
    ENDIF.
    SELECT SINGLE *
       FROM bnka
       INTO wl_bnka
       WHERE banks  = wl_lfbk-banks
       AND   bankl  = wl_lfbk-bankl.
    IF sy-subrc NE 0.
      CLEAR wl_bnka.
    ENDIF.
    wa_zfit0036_ins-bvtyp_2 = wl_lfbk-bvtyp.
    wa_zfit0036_ins-swift_2 = wl_bnka-swift.
    wa_zfit0036_ins-banks_2 = wl_lfbk-banks.
    wa_zfit0036_ins-bankl_2 = wl_lfbk-bankl.
    wa_zfit0036_ins-banka_2 = wl_bnka-banka.
    wa_zfit0036_ins-bankn_2 = wl_lfbk-bankn.

  ENDIF.

  INSERT INTO  zfit0036 VALUES wa_zfit0036_ins.
  IF sy-subrc NE 0.
    ROLLBACK WORK.
  ELSE.
    COMMIT WORK.

*  "SHDB 1-1
*  IF wg_documento IS NOT INITIAL.
*    wa_zfit0036_ins-belnr_adt_c = wg_documento.
*    PERFORM f_shdb_geradt   CHANGING wl_erro.
*    IF wl_erro = 'X'.
**      MESSAGE 'Erro ao gerar compensação' TYPE 'I'.
*    ELSE.
*      wa_zfit0036_ins-belnr_adt_g = wg_documento.
*      INSERT INTO  zfit0036 VALUES wa_zfit0036_ins.
*      IF sy-subrc NE 0.
*        ROLLBACK WORK.
*      ELSE.
*        COMMIT WORK.
*      ENDIF.
*    ENDIF.
  ENDIF.

  IF 1 = 2.
    PERFORM f_shdb_geradt   CHANGING wl_erro.
  ENDIF.
  CLEAR wa_zfit0036_ins.


ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_SHDB_GERADT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_WG_CRIAADT  text
*      <--P_WL_ERRO  text
*----------------------------------------------------------------------*
FORM f_shdb_geradt  CHANGING p_erro.
  DATA: vxblnr     TYPE bkpf-xblnr,
        vdata(10),
        wl_wrbtr   TYPE bseg-wrbtr,
        wl_vlr(16),
        wcontb     TYPE i,
        vsaknr     TYPE skat-saknr,
        wl_bsik    TYPE ty_bsik.



















  REFRESH ti_bdcdata.
  vxblnr = wa_zfit0036_ins-referencia.
  CONCATENATE  p_alv-dt_pgto+6(2) p_alv-dt_pgto+4(2) p_alv-dt_pgto(4) INTO vdata SEPARATED BY '.'.
  SELECT SINGLE  bukrs lifnr belnr dmbtr dmbe2 budat buzei gsber shkzg  umskz
          FROM bsik
          INTO wl_bsik
          WHERE bukrs EQ p_alv-bukrs
          AND   belnr EQ p_alv-belnr.

  p_alv-belnr = wg_documento.
* ---> S4 Migration - 16/06/2023 - JS
*  SELECT SINGLE wrbtr
*    INTO wl_wrbtr
*    FROM bseg
*    WHERE bukrs = p_alv-bukrs
*    AND   belnr = p_alv-belnr.
  DATA: lt_bseg TYPE fagl_t_bseg.

  CALL FUNCTION 'FAGL_GET_BSEG'
    EXPORTING
      i_bukrs = p_alv-bukrs
      i_belnr = p_alv-belnr
*<--- Inicio Alteração - MG-5592 - YA
      i_gjahr = g_gjahr
*<--- Fim Alteração - MG-5592 - YA
    IMPORTING
      et_bseg = lt_bseg
    EXCEPTIONS
      OTHERS  = 2.

  READ TABLE lt_bseg INTO DATA(wa_bseg) INDEX 1.

  IF sy-subrc EQ 0.
    wl_wrbtr = wa_bseg-wrbtr.
  ENDIF.
* <--- S4 Migration - 16/06/2023 - JS







  WRITE: wl_wrbtr TO wl_vlr.

  SELECT bukrs lifnr belnr dmbtr dmbe2 budat buzei  gsber shkzg  umskz
            FROM bsik
            INTO TABLE it_bsik_aux
            WHERE bukrs EQ p_alv-bukrs
            AND   belnr EQ p_alv-belnr.

  wcontb = 0.
  LOOP AT it_bsik_aux INTO wa_bsik.
    ADD 1 TO wcontb.
  ENDLOOP.

  SELECT  SINGLE  hkont
         FROM t012k
         INTO vsaknr
         WHERE bukrs = p_alv-bukrs
         AND   hbkid = p_alv-hbkid_e.

  PERFORM f_bdc_data USING:
    'SAPMF05A'  '0103'  'X'  ''                 ' ',
    ''          ''      ''   'BDC_OKCODE'	      '/00',
    ''          ''      ''   'BKPF-BLDAT'       vdata,
    ''          ''      ''   'BKPF-BLART'       'KZ',
    ''          ''      ''   'BKPF-BUKRS'       p_alv-bukrs,
    ''          ''      ''   'BKPF-BUDAT'       vdata,
    ''          ''      ''   'BKPF-MONAT'       p_alv-dt_pgto+4(2),
    ''          ''      ''   'BKPF-WAERS'       p_alv-moeda_pgto,
    ''          ''      ''   'BKPF-XBLNR'       vxblnr,
    ''          ''      ''   'RF05A-KONTO'      vsaknr,
    ''          ''      ''   'BSEG-GSBER'       wl_bsik-gsber,
    ''          ''      ''   'BSEG-WRBTR'       wl_vlr,
    ''          ''      ''   'RF05A-AGKON'      p_alv-lifnr,
    ''          ''      ''   'RF05A-AGKOA'      'K',
    ''          ''      ''   'RF05A-AGUMS'      wl_bsik-umskz.
  PERFORM f_bdc_data USING:
    ''          ''      ''   'RF05A-XNOPS'      'X',
    ''          ''      ''   'RF05A-XPOS1(01)'  '',
    ''          ''      ''   'RF05A-XPOS1(03)'  'X',
    'SAPMF05A'  '0731'  'X'  ''                 ' ',
    ''          ''      ''   'BDC_OKCODE'	      '=PA',
    ''          ''      ''   'RF05A-SEL01(01)'  p_alv-belnr.

  IF wcontb GT 1.
    PERFORM f_bdc_data USING:
    'SAPDF05X'  '3100'  'X'  ''                 ' ',
    ''          ''      ''   'BDC_OKCODE'	      '=PI',
    ''          ''      ''   'BDC_SUBSCR'	      'SAPDF05X                                6102PAGE',
    ''          ''      ''   'BDC_CURSOR'	      'DF05B-PSBET(01)',
    ''          ''      ''   'RF05A-ABPOS'      '1'.
  ENDIF.

  PERFORM f_bdc_data USING:
  'SAPDF05X'  '3100'  'X'  ''                 ' ',
  ''          ''      ''   'BDC_OKCODE'	      '=BU',
  ''          ''      ''   'RF05A-ABPOS'      '1'.

  CLEAR p_erro.
  PERFORM zf_call_transaction USING 'F-53' CHANGING p_erro.

  WAIT UP TO 5 SECONDS.

  IF p_erro = 'X'.
    ROLLBACK WORK.
  ELSE.
    COMMIT WORK.
  ENDIF.
ENDFORM.
