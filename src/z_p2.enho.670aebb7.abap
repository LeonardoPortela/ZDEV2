"Name: \PR:SAPLMRMP\EX:MRM_INVOICE_POST_10\EI
ENHANCEMENT 0 Z_P2.

*-CS2020001318 - 26.05.2021 - JT - inicio
  PERFORM f_calcula_zib_contabil USING t_drseg[]
                                       i_rbkpv.
*-CS2020001318 - 26.05.2021 - JT - fim

***** Busca IVA - P2
*define f_preencher_dynpro.
*  clear: tl_bdc.
*   MOVE &1 TO tl_bdc-dynbegin.
*  IF &1 = 'X'.
*    MOVE:
*     &2  TO tl_bdc-program,
*     &3  TO tl_bdc-dynpro.
*  ELSE.
*    MOVE:
*      &2 TO tl_bdc-fnam,
*      &3 TO tl_bdc-fval.
*  ENDIF.
*  APPEND tl_bdc.
*end-of-definition.
*
* field-symbols: <fs> type any.
* data: Tl_SETLEAF       TYPE TABLE OF SETLEAF WITH HEADER LINE,
*       Tl_SETLINET      TYPE TABLE OF SETLEAF WITH HEADER LINE,
*       tl_bdc            TYPE TABLE OF bdcdata with header line,
*       TL_MSG            TYPE TABLE OF BDCMSGCOLL WITH HEADER LINE,
*       tl_zib_contabil   type table of zib_contabil with header line,
*       wl_Seqitem        type sy-tabix,
*       TL_ANLA           TYPE TABLE OF ANLA WITH HEADER LINE,
*       TL_T095           TYPE TABLE OF T095 WITH HEADER LINE,
*       wl_data(10),
*       wl_anln(20),
*       wl_valor(20),
*       WL_MODE,
*       wl_par type CTU_PARAMS,
*
*       T_IVas            TYPE STANDARD TABLE OF  RGSB4 WITH HEADER LINE,
*       vlifnr            type lfa1-lifnr,
*       XUFF              type lfa1-regio,
*       XUFC              type lfa1-regio,
*       Xalf              type J_1BTXIC1-RATE,
*       XALC              type J_1BTXIC1-RATE,
*       XALIQ_DIF         type J_1BTXIC1-RATE,
*       VSEQ(10)          TYPE P,
*       VNUM(10),
*       v_SAKTO           type EKKN-SAKTO,
*       V_KOSTL           type EKKN-KOSTL.
*
*
* data: wl_shdbnr type zshdbt0001-shdbnr.
*
* if sy-tcode eq 'MIRO'.
*  CALL FUNCTION 'G_SET_GET_ALL_VALUES'
*    EXPORTING
*      CLASS         = '0000'
*      SETNR         = 'MAGGI_IVA_NLOC'
*    TABLES
*      SET_VALUES    = T_ivas
*    EXCEPTIONS
*      SET_NOT_FOUND = 1
*      OTHERS        = 2.
*  IF SY-SUBRC <> 0.
**     MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
**             WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*  ENDIF.
*  SORT T_ivas BY FROM.
*  refresh tl_zib_contabil.
*  LOOP AT  T_DRSEG.
*     read table T_IVas
*       with key from = T_DRSEG-mwskz.
*
*     IF sy-subrc = 0.
*        select single regio
*          FROM lfa1
*          into XUFF
*          where lifnr = I_RBKPV-lifnr.
*
*        if sy-subrc ne 0.
*            CONTINUE.
*        endif.
*
*        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*          EXPORTING
*            INPUT  = T_DRSEG-werks
*          IMPORTING
*            OUTPUT = vlifnr.
*
*        select single regio
*          FROM lfa1
*          into XUFC
*          where lifnr = vlifnr.
*
*        if sy-subrc ne 0.
*            CONTINUE.
*        endif.
*
*        select single rate
*          from J_1BTXIC1
*          into XALF
*          where LAND1      =  'BR'
*          and   SHIPFROM  = XUFF
*          and   SHIPTO    = XUFC.
*
*        if sy-subrc ne 0.
*          CONTINUE.
*        endif.
*
*        select single rate
*          from J_1BTXIC1
*          into XALC
*          where LAND1      =  'BR'
*          and   SHIPFROM  = XUFC
*          and   SHIPTO    = XUFC.
*
*        if sy-subrc ne 0.
*          CONTINUE.
*        endif.
*        XALIQ_DIF = XALC - XALF.
*
*        if XALIQ_DIF ne 0.
*
*           select single SAKTO KOSTL
*             from EKKN
*             into (V_SAKTO,V_KOSTL)
*           where EBELN = T_DRSEG-EBELN
*           and   EBELP = T_DRSEG-EBELP  .
*
*           CLEAR tl_zib_contabil.
*          CALL FUNCTION 'NUMBER_GET_NEXT'
*          EXPORTING
*            NR_RANGE_NR = '01'
*            OBJECT      = 'ZID_IVA'
*          IMPORTING
*            NUMBER      = VSEQ.
*          VNUM = VSEQ .
*
*        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*          EXPORTING
*            INPUT  = VNUM
*          IMPORTING
*            OUTPUT = VNUM.
*
*           CONCATENATE 'ZIVA' vnum T_DRSEG-BUDAT+0(4) into tl_zib_contabil-OBJ_KEY.
*           tl_zib_contabil-GSBER    = T_DRSEG-GSBER.
*           tl_zib_contabil-BUKRS    = T_DRSEG-BUKRS.
*           tl_zib_contabil-INTERFACE  = 35.
*           tl_zib_contabil-BKTXT    = T_DRSEG-EBELN.
*           "tl_zib_contabil-BLDAT    = I_RBKPV-BLDAT.
*           CONCATENATE I_RBKPV-BLDAT+6(2) '.' I_RBKPV-BLDAT+4(2) '.' I_RBKPV-BLDAT+0(4) into tl_zib_contabil-BLDAT.
*           "tl_zib_contabil-BUDAT    = T_DRSEG-BUDAT.
*           CONCATENATE I_RBKPV-BUDAT+6(2) '.' I_RBKPV-BUDAT+4(2) '.' I_RBKPV-BUDAT+0(4) into tl_zib_contabil-BUDAT.
*           tl_zib_contabil-GJAHR    = T_DRSEG-GJAHR.
*           tl_zib_contabil-MONAT    = I_RBKPV-BUDAT+4(2).
*           tl_zib_contabil-BLART    = 'RE'.
*           tl_zib_contabil-XBLNR    = I_RBKPV-XBLNR.
*           tl_zib_contabil-WRBTR    = I_RBKPV-RMWWR * ( XALIQ_DIF / 100 ). " Valor da fatura entrado em moeda interna (Montante bruto de fatura em moedas de documento)
*           tl_zib_contabil-WAERS    = T_DRSEG-WAERS.
*           tl_zib_contabil-BUPLA    = T_DRSEG-WERKS.
*           tl_zib_contabil-RG_ATUALIZADO =  'N'.
*
*           do 2 TIMES.
*             clear  tl_zib_contabil-KOSTL .
*             if sy-index = 1.
*                tl_zib_contabil-SEQITEM  = '000001'.
*                tl_zib_contabil-BSCHL    = '40'.
*                if T_DRSEG-mwskz = 'C7'.
*                    tl_zib_contabil-HKONT    = V_SAKTO.
*                    tl_zib_contabil-KOSTL    = V_KOSTL.
*                ELSEIF T_DRSEG-mwskz = 'Y4'.
*                    tl_zib_contabil-HKONT    = '423008'.
*                else.
*                    tl_zib_contabil-HKONT    = '121401'.
*                endif.
*             ELSE.
*                tl_zib_contabil-SEQITEM  = '000002'.
*                tl_zib_contabil-BSCHL    = '50'.
*                tl_zib_contabil-HKONT   = '213001'.
*             ENDIF.
*             APPEND tl_zib_contabil.
*           ENDDO.
*           if tl_zib_contabil[] is not INITIAL.
*             modify zib_contabil from TABLE tl_zib_contabil.
*           endif.
*
*           exit. "Faz 1 vez
*
*
*        endif.
*     ENDIF.
*  ENDLOOP.
*endif.

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
ENDENHANCEMENT.
