*******************************************************************************************
*& Report  ZFI_ZIB_CONTABIL
*******************************************************************************************
*                            AMAGGI
*******************************************************************************************
* Descrição  : Exit da Miro - Tratamento ZIB_CONTABIL
*******************************************************************************************
* Histórico das modificações
*------------------------------------------------------------------------------------------
* Data | Nome | Request | Descrição                                    *
*******************************************************************************************

***** Busca IVA - P2
DEFINE f_preencher_dynpro.
  clear: tl_bdc.
   MOVE &1 TO tl_bdc-dynbegin.
  IF &1 = 'X'.
    MOVE:
     &2  TO tl_bdc-program,
     &3  TO tl_bdc-dynpro.
  ELSE.
    MOVE:
      &2 TO tl_bdc-fnam,
      &3 TO tl_bdc-fval.
  ENDIF.
  APPEND tl_bdc.
END-OF-DEFINITION.

*******************************************************************************************
* atualiza ZIB_CONTABIL
*******************************************************************************************
FORM f_calcula_zib_contabil USING t_drseg TYPE mmcr_tdrseg
                                  i_rbkpv TYPE mrm_rbkpv.

  FIELD-SYMBOLS: <fs> TYPE any.
  DATA: tl_setleaf      TYPE TABLE OF setleaf WITH HEADER LINE,
        tl_setlinet     TYPE TABLE OF setleaf WITH HEADER LINE,
        tl_bdc          TYPE TABLE OF bdcdata WITH HEADER LINE,
        tl_msg          TYPE TABLE OF bdcmsgcoll WITH HEADER LINE,
        tl_zib_contabil TYPE TABLE OF zib_contabil WITH HEADER LINE,
        wl_seqitem      TYPE sy-tabix,
        tl_anla         TYPE TABLE OF anla WITH HEADER LINE,
        tl_t095         TYPE TABLE OF t095 WITH HEADER LINE,
        wl_data(10),
        wl_anln(20),
        wl_valor(20),
        wl_mode,
        wl_par          TYPE ctu_params,
        wl_wrbtr        TYPE zib_contabil-wrbtr,
*
        t_ivas          TYPE STANDARD TABLE OF  rgsb4 WITH HEADER LINE,
        vlifnr          TYPE lfa1-lifnr,
        xuff            TYPE lfa1-regio,
        xufc            TYPE lfa1-regio,
        xalf            TYPE j_1btxic1-rate,
        xalc            TYPE j_1btxic1-rate,
        xaliq_dif       TYPE j_1btxic1-rate,
        vseq(10)        TYPE p,
        vnum(10),
        v_sakto         TYPE ekkn-sakto,
        v_kostl         TYPE ekkn-kostl.

  DATA: wl_shdbnr TYPE zshdbt0001-shdbnr.

  IF sy-tcode EQ 'MIRO'.
    CALL FUNCTION 'G_SET_GET_ALL_VALUES'
      EXPORTING
        class           = '0000'
        setnr           = 'MAGGI_IVA_NLOC'
        no_descriptions = abap_false
      TABLES
        set_values      = t_ivas
      EXCEPTIONS
        set_not_found   = 1
        OTHERS          = 2.
    IF sy-subrc <> 0.
*     MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*             WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.
    SORT t_ivas BY from.
    REFRESH tl_zib_contabil.
    LOOP AT  t_drseg INTO DATA(w_drseg).
      READ TABLE t_ivas
        WITH KEY from = w_drseg-mwskz.

      IF sy-subrc = 0 AND  t_ivas-title IS INITIAL.
        SELECT SINGLE regio
          FROM lfa1
          INTO xuff
         WHERE lifnr = i_rbkpv-lifnr.

        IF sy-subrc NE 0.
          CONTINUE.
        ENDIF.

        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = w_drseg-werks
          IMPORTING
            output = vlifnr.

        SELECT SINGLE regio
          FROM lfa1
          INTO xufc
         WHERE lifnr = vlifnr.

        IF sy-subrc NE 0.
          CONTINUE.
        ENDIF.

        SELECT SINGLE rate
          FROM j_1btxic1
          INTO xalf
         WHERE land1     = 'BR'
           AND shipfrom  = xuff
           AND shipto    = xufc.

        IF sy-subrc NE 0.
          CONTINUE.
        ENDIF.

        SELECT SINGLE rate
          FROM j_1btxic1
          INTO xalc
         WHERE land1     = 'BR'
           AND shipfrom  = xufc
           AND shipto    = xufc.

        IF sy-subrc NE 0.
          CONTINUE.
        ENDIF.

*-CS2020001318 - 26.05.2021 - JT - inicio
        IF t_ivas-title = 'BZ'.
          PERFORM f_icms_base_dupla CHANGING wl_wrbtr.
        ELSE.
          xaliq_dif = xalc - xalf.
          wl_wrbtr  = i_rbkpv-rmwwr * ( xaliq_dif / 100 ).
        ENDIF.
*-CS2020001318 - 26.05.2021 - JT - fim

*       IF xaliq_dif NE 0.
        IF wl_wrbtr NE 0.

          SELECT SINGLE sakto kostl
            FROM ekkn
            INTO (v_sakto,v_kostl)
          WHERE ebeln = w_drseg-ebeln
          AND   ebelp = w_drseg-ebelp  .

          CLEAR tl_zib_contabil.
          CALL FUNCTION 'NUMBER_GET_NEXT'
            EXPORTING
              nr_range_nr = '01'
              object      = 'ZID_IVA'
            IMPORTING
              number      = vseq.
          vnum = vseq .

          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
            EXPORTING
              input  = vnum
            IMPORTING
              output = vnum.

          CONCATENATE 'ZIVA' vnum w_drseg-budat+0(4) INTO tl_zib_contabil-obj_key.
          tl_zib_contabil-gsber    = w_drseg-gsber.
          tl_zib_contabil-bukrs    = w_drseg-bukrs.
          tl_zib_contabil-interface  = 35.
          tl_zib_contabil-bktxt    = w_drseg-ebeln.
          "tl_zib_contabil-BLDAT    = I_RBKPV-BLDAT.
          CONCATENATE i_rbkpv-bldat+6(2) '.' i_rbkpv-bldat+4(2) '.' i_rbkpv-bldat+0(4) INTO tl_zib_contabil-bldat.
          "tl_zib_contabil-BUDAT    = T_DRSEG-BUDAT.
          CONCATENATE i_rbkpv-budat+6(2) '.' i_rbkpv-budat+4(2) '.' i_rbkpv-budat+0(4) INTO tl_zib_contabil-budat.
          tl_zib_contabil-gjahr    = w_drseg-gjahr.
          tl_zib_contabil-monat    = i_rbkpv-budat+4(2).
          tl_zib_contabil-blart    = 'RE'.
          tl_zib_contabil-xblnr    = i_rbkpv-xblnr.
          tl_zib_contabil-wrbtr    = wl_wrbtr.
*         tl_zib_contabil-wrbtr    = i_rbkpv-rmwwr * ( xaliq_dif / 100 ). " Valor da fatura entrado em moeda interna (Montante bruto de fatura em moedas de documento)
          tl_zib_contabil-waers    = w_drseg-waers.
          tl_zib_contabil-bupla    = w_drseg-werks.
          tl_zib_contabil-rg_atualizado =  'N'.

          DO 2 TIMES.
            CLEAR  tl_zib_contabil-kostl .
            IF sy-index = 1.
              tl_zib_contabil-seqitem  = '000001'.
              tl_zib_contabil-bschl    = '40'.

              CASE w_drseg-mwskz.
                WHEN 'C7'.
                  tl_zib_contabil-hkont    = v_sakto.
                  tl_zib_contabil-kostl    = v_kostl.
                WHEN 'Y4' OR 'Y5'.
                  tl_zib_contabil-hkont    = '423008'.
                WHEN OTHERS.
                  tl_zib_contabil-hkont    = '121401'.
              ENDCASE.

*              IF w_drseg-mwskz = 'C7'.
*                tl_zib_contabil-hkont    = v_sakto.
*                tl_zib_contabil-kostl    = v_kostl.
*              ELSEIF w_drseg-mwskz = 'Y4'.
*                tl_zib_contabil-hkont    = '423008'.
*              ELSE.
*                tl_zib_contabil-hkont    = '121401'.
*              ENDIF.

            ELSE.
              tl_zib_contabil-seqitem  = '000002'.
              tl_zib_contabil-bschl    = '50'.
              tl_zib_contabil-hkont   = '213001'.
            ENDIF.
            APPEND tl_zib_contabil.
          ENDDO.
          IF tl_zib_contabil[] IS NOT INITIAL.
            MODIFY zib_contabil FROM TABLE tl_zib_contabil.
          ENDIF.

          EXIT. "Faz 1 vez

        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDIF.

  " IR32488 ALRS

* if sy-tcode eq 'MIRO'
* and i_simulation is initial.
*   assign ('(SAPLFDCB)INVFO-GSBER') to <fs>.
*    if <fs> is assigned.
*      SELECT *
*        FROM SETLEAF
*        INTO TABLE Tl_SETLEAF
*         WHERE SETNAME EQ 'MAGGI_IVA_IMOBILIZADO'.
*
*  if sy-subrc is initial.
*     read table t_bset
*       with key HKONT = '0000213001'.
*
*     read table tl_setleaf
*       with key valfrom = t_bset-mwskz.
*
*     if sy-subrc is initial.
*
*    IF T_DRSEG[] IS NOT INITIAL.
*      SELECT *
*        FROM ANLA
*        INTO TABLE TL_ANLA
*         FOR ALL ENTRIES IN T_DRSEG
*          WHERE BUKRS EQ T_DRSEG-BUKRS
*            AND ANLN1 EQ T_DRSEG-ANLN1
*            AND ANLN2 EQ T_DRSEG-ANLN2.
*
*      IF SY-SUBRC IS INITIAL .
*        SELECT *
*          FROM T095
*          INTO TABLE TL_T095
*           FOR ALL ENTRIES IN TL_ANLA
*            WHERE KTOPL EQ '0050'
*              AND KTOGR EQ TL_ANLA-KTOGR
*              AND AFABE EQ '01'.
*      ENDIF.
*    ENDIF.
*
*LOOP AT T_DRSEG.
*  IF t_drseg-bpmng eq 0.
*      CONTINUE.
*  ENDIF.
*  REFRESH: TL_BDC.
*  REFRESH: TL_zib_contabil.
*  clear: wl_seqitem, tl_zib_contabil, TL_T095, TL_ANLA.
*write: t_drseg-BUDAT to wl_data,
*      t_bset-hwste to wl_valor.
*concatenate t_drseg-anln1 t_drseg-anln2 into wl_anln separated by '-'.
*condense: wl_anln no-gaps,
*          wl_valor no-gaps.
*
*wl_par-dismode = 'N'.
*wl_par-updmode = 'A'.
*wl_par-defsize = 'X'.
*wl_par-racommit = 'X'.
*wl_par-NOBINPT = 'X'.
*
**  LOOP AT t_saida INTO wa_saida.
*    f_preencher_dynpro:
*    'X' 'SAPMF05A'             '0100',
*    ' ' 'BKPF-BLDAT'          wl_data,
*    ' ' 'BKPF-BLART'          'RE',
*    ' ' 'BKPF-BUKRS'         t_bset-bukrs,  "empresa
*    ' ' 'BKPF-BUDAT'          wl_data,
*    ' ' 'BKPF-MONAT'          wl_data+3(2),
*    ' ' 'BKPF-WAERS'          'BRL',
*    ' ' 'BKPF-XBLNR'          t_DRSEG-ebeln,
*    ' ' 'RF05A-NEWKO'          wl_anln,
*    ' ' 'RF05A-NEWBS'          '70',
*    ' ' 'RF05A-NEWBW'          '100',
*    ' ' 'BDC_OKCODE'           '/00',
*
*
*    'X' 'SAPMF05A'             '0305',
*    ' ' 'BSEG-WRBTR'           wl_valor, "t_bset-HWSTE,
*    ' ' 'BSEG-BUPLA'           <FS>,
*    ' ' 'RF05A-NEWKO'          '0000121401',
*    ' ' 'RF05A-NEWBS'          '50',
*    ' ' 'BDC_OKCODE'           '/00',
*
**    'X' 'SAPLKACB'             '0002',
**    ' ' 'BDC_OKCODE'           '=ESC',
*
*    'X' 'SAPMF05A'             '0300',
*    ' ' 'BSEG-WRBTR'           wl_valor, "t_bcset-HWSTE,
*    ' ' 'BSEG-ZUONR'           RBKPV-XBLNR, "t_bset-HWSTE,
*    ' ' 'BSEG-BUPLA'           <FS>,
*    ' ' 'BDC_OKCODE'           '=BU'.
**
*
*CALL FUNCTION 'ZSHDB_CRIA_EXECUTA'
*  EXPORTING
*    TCODE                           = 'F-02'
*    PARAMS                          = wl_par
* IMPORTING
*   SHDBNR                          = wl_shdbnr
*  TABLES
*    T_BDC                           = tl_bdc .
*
*  WAIT up to 10 SECONDS   .
*  ENDLOOP.
*     endif.
*   endif.
*   endif.
* endif.

ENDFORM.

*******************************************************************************************
* calculo icms base dupla
*******************************************************************************************
FORM f_icms_base_dupla CHANGING p_wrbtr.

  FIELD-SYMBOLS: <f_price> TYPE ANY TABLE.

  DATA: t_xkomv TYPE TABLE OF komv_index,
        w_xkomv TYPE komv_index,
        l_bx12  TYPE komv_index-kwert,
        l_bic0  TYPE komv_index-kwert,
        l_bich  TYPE komv_index-kwert,
        l_xv1   TYPE komv_index-kwert,
        l_xv2   TYPE komv_index-kwert,
        l_xv3   TYPE komv_index-kwert,
        l_xv4   TYPE komv_index-kwert,
        l_xv5   TYPE komv_index-kwert.

  FREE: p_wrbtr.

  ASSIGN ('(SAPLV61A)XKOMV[]')   TO <f_price>[].

  CHECK sy-subrc = 0.

  t_xkomv[] = <f_price>[].

  FREE: l_bx12, l_bic0, l_bich,
        l_xv1,  l_xv2,  l_xv3,  l_xv4, l_xv5.

*--------------------------
* recupera BX12
*--------------------------
  READ TABLE t_xkomv INTO w_xkomv WITH KEY kschl = 'BX12'.
  IF sy-subrc = 0.
    l_bx12 = w_xkomv-kwert.
  ENDIF.

*--------------------------
* recupera BIC0
*--------------------------
  READ TABLE t_xkomv INTO w_xkomv WITH KEY kschl = 'BIC0'.
  IF sy-subrc = 0.
    l_bic0 = w_xkomv-kwert / 10.
  ENDIF.

*--------------------------
* recupera BICH
*--------------------------
  READ TABLE t_xkomv INTO w_xkomv WITH KEY kschl = 'BICH'.
  IF sy-subrc = 0.
    l_bich = w_xkomv-kwert / 10.
  ENDIF.

*--------------------------
* calculo icms base dupla
*--------------------------
  l_xv1   = l_bx12 * l_bic0.
  l_xv2   = l_bx12 - l_xv1.

  IF l_bich = 1.
    l_xv3 = l_xv2.
  ELSE.
    l_xv3 = l_xv2  / ( 1 - l_bich ).
  ENDIF.

  l_xv4   = l_xv3  * l_bich.
  l_xv5   = l_xv4  - l_xv1.
  p_wrbtr = l_xv5.

ENDFORM.
