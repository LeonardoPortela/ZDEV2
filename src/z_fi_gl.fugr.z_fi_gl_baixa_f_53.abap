FUNCTION Z_FI_GL_BAIXA_F_53.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  TABLES
*"      IT_COMPENSAR STRUCTURE  ZDE_DOC_VALOR
*"      IT_RETORNO STRUCTURE  BDCMSGCOLL OPTIONAL
*"      IT_BKPF_RET STRUCTURE  ZDE_DOC_VALOR OPTIONAL
*"      IT_BKPF_RET2 STRUCTURE  BAPIRET2 OPTIONAL
*"  EXCEPTIONS
*"      ERRO_BLOQUEIO
*"----------------------------------------------------------------------
  DATA: IT_MESSAGE   LIKE BDCMSGCOLL OCCURS 0 WITH HEADER LINE,
        WA_RETORNO   TYPE BDCMSGCOLL,
        IT_BSEG      TYPE TABLE OF BSEG WITH HEADER LINE,
        WA_BKPF      TYPE ZDE_DOC_VALOR,
        IT_LFB1      TYPE TABLE OF LFB1 WITH HEADER LINE,
        IT_TOTALIZA  TYPE TABLE OF ZDE_TOTALIZA_DOC WITH HEADER LINE,
        LC_DOCUMENTO TYPE I,
        CK_CONTINUAR TYPE C LENGTH 1,
        LC_GSBER     TYPE GSBER,
        LC_DOC_STR   TYPE ZCHAR02,
        LC_MODE      TYPE CHAR01,
        LC_DMBTR     TYPE DMBTR,
        LC_DMBTR_RES TYPE DMBTR,
        I            TYPE I.

  DATA: BEGIN OF TI_BDCDATA OCCURS 0.
          INCLUDE STRUCTURE BDCDATA.
  DATA: END OF TI_BDCDATA.

  CLEAR: IT_BKPF_RET[], IT_TOTALIZA[].

  CHECK IT_COMPENSAR[] IS NOT INITIAL.

  CALL FUNCTION 'FAGL_GET_BSEG_FOR_ALL_ENTRIES'
    EXPORTING IT_FOR_ALL_ENTRIES = IT_COMPENSAR
              I_WHERE_CLAUSE = |BUKRS EQ IT_FOR_ALL_ENTRIES-BUKRS AND BELNR EQ IT_FOR_ALL_ENTRIES-BELNR AND GJAHR EQ IT_FOR_ALL_ENTRIES-GJAHR AND KOART EQ IT_FOR_ALL_ENTRIES-KOART|
    IMPORTING ET_BSEG = IT_BSEG[]
    EXCEPTIONS NOT_FOUND = 1.
IF SY-SUBRC = 0 AND LINES( IT_BSEG[] ) > 0.
  MOVE-CORRESPONDING IT_BSEG[] TO IT_BSEG[].
  SY-DBCNT = LINES( IT_BSEG[] ).
ELSE.
  SY-SUBRC = 4.
  SY-DBCNT = 0.
ENDIF.


  SORT IT_BSEG BY BUKRS BELNR GJAHR.

  CHECK IT_BSEG[] IS NOT INITIAL.

  LOOP AT IT_BSEG.
    IF IT_BSEG-ZLSPR IS NOT INITIAL.
      MESSAGE E035(ZFI) WITH IT_BSEG-BELNR RAISING ERRO_BLOQUEIO.
    ENDIF.
  ENDLOOP.

  SELECT * INTO TABLE IT_LFB1
    FROM LFB1
     FOR ALL ENTRIES IN IT_BSEG
   WHERE LIFNR EQ IT_BSEG-LIFNR
     AND BUKRS EQ IT_BSEG-BUKRS.

  LOOP AT IT_LFB1.
    IF IT_LFB1-ZAHLS IS NOT INITIAL.
      MESSAGE E671(F5) WITH IT_LFB1-LIFNR IT_LFB1-ZAHLS RAISING ERRO_BLOQUEIO.
    ENDIF.
  ENDLOOP.

  "Totalizar """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
  """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
  SORT IT_COMPENSAR BY PARID.
  FIELD-SYMBOLS <DSCOMPE> TYPE ZDE_DOC_VALOR.
  FIELD-SYMBOLS <DSTOTA>  TYPE ZDE_TOTALIZA_DOC.
  LC_DOCUMENTO = 0.

  LOOP AT IT_COMPENSAR ASSIGNING <DSCOMPE>.

    CLEAR: LC_GSBER, CK_CONTINUAR.

    READ TABLE IT_BSEG WITH KEY BUKRS = IT_COMPENSAR-BUKRS BELNR = IT_COMPENSAR-BELNR GJAHR = IT_COMPENSAR-GJAHR BINARY SEARCH.
    IF SY-SUBRC IS NOT INITIAL.
      CONTINUE.
    ELSE.
      LC_GSBER = IT_BSEG-GSBER.
    ENDIF.

    "Somente concatenar um no contador, não pode passar de 10
    CLEAR: IT_TOTALIZA.
    LOOP AT IT_TOTALIZA ASSIGNING <DSTOTA>
         WHERE KONTO     EQ <DSCOMPE>-KONTO     "Nº conta ou matchcode para conta a ser contabilizada
           AND BUKRS     EQ <DSCOMPE>-BUKRS     "Empresa
           AND GSBER     EQ LC_GSBER            "Divisão
           AND PARID     EQ <DSCOMPE>-PARID     "Identificação do parceiro (cliente, fornecedor, loc.negócio)
           AND KOART     EQ <DSCOMPE>-KOART     "Tipo de conta
           AND ZFBDT_RES EQ <DSCOMPE>-ZFBDT_RES "Data base para cálculo do vencimento
           AND WAERS     EQ <DSCOMPE>-WAERS     "Código da moeda
           AND BUDAT     EQ <DSCOMPE>-BUDAT     "Data de Lançamento
           AND BLDAT     EQ <DSCOMPE>-BLDAT     "Data do Documento
           AND SHKZG     EQ <DSCOMPE>-SHKZG.    "Código débito/crédito
      "AND CONTADOR  LT 10.
      "AND CONTADOR  LT 10 -- caso tenha limitador de quantidade colocar aqui
      CK_CONTINUAR = 'X'.
      ADD 1                   TO <DSTOTA>-CONTADOR.
      IF ( <DSCOMPE>-KOART EQ 'D' AND <DSCOMPE>-SHKZG EQ 'S' ) OR "Cliente/Debito
         ( <DSCOMPE>-KOART EQ 'K' AND <DSCOMPE>-SHKZG EQ 'H' ).   "Fornecedor/Credito
        ADD <DSCOMPE>-DMBTR     TO <DSTOTA>-DMBTR.
        ADD <DSCOMPE>-DMBTR_RES TO <DSTOTA>-DMBTR_RES.
      ELSEIF ( <DSCOMPE>-KOART EQ 'D' AND <DSCOMPE>-SHKZG EQ 'H' ) OR "Cliente/Credito
             ( <DSCOMPE>-KOART EQ 'K' AND <DSCOMPE>-SHKZG EQ 'S' ).   "Fornecedor/Debito
        LC_DMBTR     = <DSCOMPE>-DMBTR     * -1.
        LC_DMBTR_RES = <DSCOMPE>-DMBTR_RES * -1.
        ADD LC_DMBTR     TO <DSTOTA>-DMBTR.
        ADD LC_DMBTR_RES TO <DSTOTA>-DMBTR_RES.
      ENDIF.
      <DSCOMPE>-DOCUMENTO = <DSTOTA>-DOCUMENTO.
    ENDLOOP.

    "Cria um novo documento
    IF CK_CONTINUAR NE 'X'.
      ADD 1 TO LC_DOCUMENTO.
      MOVE LC_DOCUMENTO TO <DSCOMPE>-DOCUMENTO.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          INPUT  = <DSCOMPE>-DOCUMENTO
        IMPORTING
          OUTPUT = <DSCOMPE>-DOCUMENTO.
      IT_TOTALIZA-KONTO     = <DSCOMPE>-KONTO.
      IT_TOTALIZA-BUKRS     = <DSCOMPE>-BUKRS.
      IT_TOTALIZA-GSBER     = LC_GSBER.
      IT_TOTALIZA-PARID     = <DSCOMPE>-PARID.
      IT_TOTALIZA-KOART     = <DSCOMPE>-KOART.
      IT_TOTALIZA-ZFBDT_RES = <DSCOMPE>-ZFBDT_RES.
      IT_TOTALIZA-WAERS     = <DSCOMPE>-WAERS.
      IT_TOTALIZA-BUDAT     = <DSCOMPE>-BUDAT.
      IT_TOTALIZA-BLDAT     = <DSCOMPE>-BLDAT.
      IT_TOTALIZA-DOCUMENTO = <DSCOMPE>-DOCUMENTO.
      IT_TOTALIZA-SHKZG     = <DSCOMPE>-SHKZG.

      IF ( <DSCOMPE>-KOART EQ 'D' AND <DSCOMPE>-SHKZG EQ 'S' ) OR "Cliente/Debito
         ( <DSCOMPE>-KOART EQ 'K' AND <DSCOMPE>-SHKZG EQ 'H' ).   "Fornecedor/Credito
        IT_TOTALIZA-DMBTR     = <DSCOMPE>-DMBTR.
        IT_TOTALIZA-DMBTR_RES = <DSCOMPE>-DMBTR_RES.
      ELSEIF ( <DSCOMPE>-KOART EQ 'D' AND <DSCOMPE>-SHKZG EQ 'H' ) OR "Cliente/Credito
             ( <DSCOMPE>-KOART EQ 'K' AND <DSCOMPE>-SHKZG EQ 'S' ).   "Fornecedor/Debito
        LC_DMBTR     = <DSCOMPE>-DMBTR     * -1.
        LC_DMBTR_RES = <DSCOMPE>-DMBTR_RES * -1.
        IT_TOTALIZA-DMBTR     = LC_DMBTR.
        IT_TOTALIZA-DMBTR_RES = LC_DMBTR_RES.
      ENDIF.

      IT_TOTALIZA-CONTADOR  = 1.

      CASE <DSCOMPE>-KOART.
        WHEN 'K'.
          SELECT SINGLE NAME1 INTO IT_TOTALIZA-NAME1 FROM LFA1 WHERE LIFNR EQ <DSCOMPE>-PARID.
        WHEN 'D'.
          SELECT SINGLE NAME1 INTO IT_TOTALIZA-NAME1 FROM KNA1 WHERE KUNNR EQ <DSCOMPE>-PARID.
      ENDCASE.

      APPEND IT_TOTALIZA.
    ENDIF.
  ENDLOOP.
  """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

  LOOP AT IT_TOTALIZA.

    CLEAR TI_BDCDATA[].

    "Tela Principal """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    CLEAR TI_BDCDATA.
    MOVE:  'SAPMF05A'       TO TI_BDCDATA-PROGRAM,
           '0103'	          TO TI_BDCDATA-DYNPRO,
           'X'              TO TI_BDCDATA-DYNBEGIN.
    APPEND TI_BDCDATA.

    CLEAR TI_BDCDATA.
    MOVE: 'BDC_OKCODE'      TO    TI_BDCDATA-FNAM,
          '/00' TO    TI_BDCDATA-FVAL.
    APPEND TI_BDCDATA.
    CLEAR TI_BDCDATA.

    CLEAR TI_BDCDATA.
    MOVE: 'BKPF-BLDAT'       TO TI_BDCDATA-FNAM.
    CONCATENATE IT_TOTALIZA-BLDAT+6(2)
                IT_TOTALIZA-BLDAT+4(2)
                IT_TOTALIZA-BLDAT(4) INTO TI_BDCDATA-FVAL.
    APPEND TI_BDCDATA.

    CLEAR TI_BDCDATA.
    IF IT_TOTALIZA-KOART EQ 'K'.
      MOVE: 'BKPF-BLART'       TO TI_BDCDATA-FNAM,
            'KZ'               TO TI_BDCDATA-FVAL.
    ELSE.
      MOVE: 'BKPF-BLART'       TO TI_BDCDATA-FNAM,
            'DZ'               TO TI_BDCDATA-FVAL.
    ENDIF.
    APPEND TI_BDCDATA.

    CLEAR TI_BDCDATA.
    MOVE: 'BKPF-BUKRS'       TO TI_BDCDATA-FNAM,
          IT_TOTALIZA-BUKRS TO TI_BDCDATA-FVAL.
    APPEND TI_BDCDATA.

    CLEAR TI_BDCDATA.
    MOVE: 'BKPF-BUDAT'       TO TI_BDCDATA-FNAM.
    CONCATENATE IT_TOTALIZA-BUDAT+6(2)
                IT_TOTALIZA-BUDAT+4(2)
                IT_TOTALIZA-BUDAT(4) INTO TI_BDCDATA-FVAL.
    APPEND TI_BDCDATA.

    CLEAR TI_BDCDATA.
    MOVE: 'BSEG-VALUT'       TO TI_BDCDATA-FNAM.
    CONCATENATE IT_TOTALIZA-BUDAT+6(2)
                IT_TOTALIZA-BUDAT+4(2)
                IT_TOTALIZA-BUDAT(4) INTO TI_BDCDATA-FVAL.
    APPEND TI_BDCDATA.

    CLEAR TI_BDCDATA.
    MOVE: 'BKPF-MONAT'            TO TI_BDCDATA-FNAM,
          IT_TOTALIZA-BUDAT+4(2)  TO TI_BDCDATA-FVAL.
    APPEND TI_BDCDATA.

    CLEAR TI_BDCDATA.
    MOVE: 'BKPF-WAERS'       TO TI_BDCDATA-FNAM,
          IT_TOTALIZA-WAERS  TO TI_BDCDATA-FVAL.
    APPEND TI_BDCDATA.

    CLEAR TI_BDCDATA.
    MOVE: 'RF05A-AUGTX'      TO TI_BDCDATA-FNAM.
    CASE IT_TOTALIZA-KOART.
      WHEN 'K'.
        MESSAGE S037(ZFI) WITH  IT_TOTALIZA-NAME1 INTO TI_BDCDATA-FVAL.
      WHEN 'D'.
        MESSAGE S038(ZFI) WITH  IT_TOTALIZA-NAME1 INTO TI_BDCDATA-FVAL.
    ENDCASE.
    I = STRLEN( TI_BDCDATA-FVAL ).
    IF I GT 50.
      TI_BDCDATA-FVAL = TI_BDCDATA-FVAL(50).
    ENDIF.
    APPEND TI_BDCDATA.

    CLEAR TI_BDCDATA.
    MOVE: 'RF05A-KONTO'      TO TI_BDCDATA-FNAM,
          IT_TOTALIZA-KONTO  TO TI_BDCDATA-FVAL.
    APPEND TI_BDCDATA.

    CLEAR TI_BDCDATA.
    MOVE: 'BSEG-GSBER'       TO TI_BDCDATA-FNAM,
          IT_TOTALIZA-GSBER  TO TI_BDCDATA-FVAL.
    APPEND TI_BDCDATA.

    CLEAR TI_BDCDATA.
    MOVE: 'BSEG-WRBTR'        TO TI_BDCDATA-FNAM.
    WA_BKPF-DMBTR = ABS( IT_TOTALIZA-DMBTR ).
    MOVE WA_BKPF-DMBTR TO TI_BDCDATA-FVAL.
    CONDENSE TI_BDCDATA-FVAL NO-GAPS.

    CALL FUNCTION 'STRING_REPLACE'
      EXPORTING
        PATTERN    = '.'
        SUBSTITUTE = ','
      CHANGING
        TEXT       = TI_BDCDATA-FVAL.

    APPEND TI_BDCDATA.

    CLEAR TI_BDCDATA.
    MOVE: 'RF05A-AGKON'       TO TI_BDCDATA-FNAM,
           IT_TOTALIZA-PARID  TO TI_BDCDATA-FVAL.
    APPEND TI_BDCDATA.

    CLEAR TI_BDCDATA.
    MOVE: 'RF05A-AGKOA'       TO TI_BDCDATA-FNAM,
          IT_TOTALIZA-KOART   TO TI_BDCDATA-FVAL.
    APPEND TI_BDCDATA.

    CLEAR TI_BDCDATA.
    MOVE: 'RF05A-XNOPS'     TO TI_BDCDATA-FNAM,
           'X'              TO TI_BDCDATA-FVAL.
    APPEND TI_BDCDATA.
    "                                                  RF05A-XPOS1(01)
    CLEAR TI_BDCDATA.
    MOVE: 'RF05A-XPOS1(03)' TO TI_BDCDATA-FNAM,
           'X'              TO TI_BDCDATA-FVAL.
    APPEND TI_BDCDATA.

    "Etapa de Apontamento do documento """"""""""""""""""""""""""""""""""""""""""""""""""""
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    "A cada 5 documentos dar um enter para gravar
    LC_DOCUMENTO = 1.

    LOOP AT IT_COMPENSAR WHERE DOCUMENTO EQ IT_TOTALIZA-DOCUMENTO.

      IF LC_DOCUMENTO = 1.
        CLEAR TI_BDCDATA.
        MOVE:  'SAPMF05A'       TO TI_BDCDATA-PROGRAM,
               '0731'	          TO TI_BDCDATA-DYNPRO,
               'X'              TO TI_BDCDATA-DYNBEGIN.
        APPEND TI_BDCDATA.

        CLEAR TI_BDCDATA.
        MOVE: 'BDC_OKCODE'      TO TI_BDCDATA-FNAM,
              '/00'             TO TI_BDCDATA-FVAL.
        APPEND TI_BDCDATA.
        CLEAR TI_BDCDATA.
      ENDIF.

      MOVE LC_DOCUMENTO TO LC_DOC_STR.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          INPUT  = LC_DOC_STR
        IMPORTING
          OUTPUT = LC_DOC_STR.

      CLEAR TI_BDCDATA.
      CONCATENATE 'RF05A-SEL01(' LC_DOC_STR ')' INTO TI_BDCDATA-FNAM.
      MOVE IT_COMPENSAR-BELNR TO TI_BDCDATA-FVAL.
      APPEND TI_BDCDATA.

      IF LC_DOCUMENTO EQ 5.
        LC_DOCUMENTO = 1.
      ELSE.
        ADD 1 TO LC_DOCUMENTO.
      ENDIF.
    ENDLOOP.
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

    " Processar PA """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    CLEAR TI_BDCDATA.
    MOVE:  'SAPMF05A'       TO TI_BDCDATA-PROGRAM,
           '0731'	          TO TI_BDCDATA-DYNPRO,
           'X'              TO TI_BDCDATA-DYNBEGIN.
    APPEND TI_BDCDATA.

    CLEAR TI_BDCDATA.
    MOVE: 'BDC_OKCODE'      TO TI_BDCDATA-FNAM,
          '=PA'             TO TI_BDCDATA-FVAL.
    APPEND TI_BDCDATA.
    CLEAR TI_BDCDATA.
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

    "Lançamento de Valor Residual """""""""""""""""""""""""""""""""""""""""""""""""""""""""
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    IF IT_TOTALIZA-DMBTR_RES NE 0 AND IT_TOTALIZA-ZFBDT_RES IS NOT INITIAL.

      "Tela de Documentos - Residual """"""""""""""""
      CLEAR TI_BDCDATA.
      MOVE:  'SAPDF05X'       TO TI_BDCDATA-PROGRAM,
             '3100'	          TO TI_BDCDATA-DYNPRO,
             'X'              TO TI_BDCDATA-DYNBEGIN.
      APPEND TI_BDCDATA.

      CLEAR TI_BDCDATA.
      MOVE: 'BDC_OKCODE'      TO TI_BDCDATA-FNAM,
            '=REST'           TO TI_BDCDATA-FVAL.
      APPEND TI_BDCDATA.

      CLEAR TI_BDCDATA.
      MOVE: 'RF05A-ABPOS'     TO TI_BDCDATA-FNAM,
            '1'               TO TI_BDCDATA-FVAL.
      APPEND TI_BDCDATA.

      "Dublo click """"""""""""""""""""""""""""""""""
      CLEAR TI_BDCDATA.
      MOVE:  'SAPDF05X'       TO TI_BDCDATA-PROGRAM,
             '3100'	          TO TI_BDCDATA-DYNPRO,
             'X'              TO TI_BDCDATA-DYNBEGIN.
      APPEND TI_BDCDATA.

      CLEAR TI_BDCDATA.
      MOVE: 'BDC_OKCODE'      TO TI_BDCDATA-FNAM,
            '=PI'             TO TI_BDCDATA-FVAL.
      APPEND TI_BDCDATA.

      CLEAR TI_BDCDATA.
      MOVE: 'BDC_CURSOR'      TO TI_BDCDATA-FNAM,
            'DF05B-PSDIF(01)' TO TI_BDCDATA-FVAL.
      APPEND TI_BDCDATA.

      CLEAR TI_BDCDATA.
      MOVE: 'RF05A-ABPOS'     TO TI_BDCDATA-FNAM,
            '1'               TO TI_BDCDATA-FVAL.
      APPEND TI_BDCDATA.

      "Simular """""
      CLEAR TI_BDCDATA.
      MOVE:  'SAPDF05X'       TO TI_BDCDATA-PROGRAM,
             '3100'	          TO TI_BDCDATA-DYNPRO,
             'X'              TO TI_BDCDATA-DYNBEGIN.
      APPEND TI_BDCDATA.

      CLEAR TI_BDCDATA.
      MOVE: 'BDC_OKCODE'      TO TI_BDCDATA-FNAM,
            '=BS'             TO TI_BDCDATA-FVAL.
      APPEND TI_BDCDATA.

      "Partida Residual """""""""""""""""""""""""""""
      CLEAR TI_BDCDATA.
      MOVE:  'SAPMF05A'       TO TI_BDCDATA-PROGRAM,
             '0700'	          TO TI_BDCDATA-DYNPRO,
             'X'              TO TI_BDCDATA-DYNBEGIN.
      APPEND TI_BDCDATA.

      CLEAR TI_BDCDATA.
      MOVE: 'BDC_CURSOR'      TO TI_BDCDATA-FNAM,
            'RF05A-AZEI1(02)' TO TI_BDCDATA-FVAL.
      APPEND TI_BDCDATA.

      CLEAR TI_BDCDATA.
      MOVE: 'BDC_OKCODE'      TO TI_BDCDATA-FNAM,
            '=PI'             TO TI_BDCDATA-FVAL.
      APPEND TI_BDCDATA.


      "Tela de partida residual """""""""""""""""""""
      CLEAR TI_BDCDATA.
      MOVE:  'SAPMF05A' TO TI_BDCDATA-PROGRAM.
      MOVE:  'X'        TO TI_BDCDATA-DYNBEGIN.
      IF IT_TOTALIZA-SHKZG EQ 'S' AND IT_TOTALIZA-KOART EQ 'K'.
        "F-28
        MOVE: '0301'    TO TI_BDCDATA-DYNPRO.
      ELSEIF IT_TOTALIZA-SHKZG EQ 'H' AND IT_TOTALIZA-KOART EQ 'K'.
        "F-53
        MOVE: '0302'    TO TI_BDCDATA-DYNPRO.
      ELSEIF IT_TOTALIZA-SHKZG EQ 'S' AND IT_TOTALIZA-KOART EQ 'D'.
        "F-28
        MOVE: '0301'    TO TI_BDCDATA-DYNPRO.
      ELSEIF IT_TOTALIZA-SHKZG EQ 'H' AND IT_TOTALIZA-KOART EQ 'D'.
        "F-53
        MOVE: '0302'    TO TI_BDCDATA-DYNPRO.
      ENDIF.

      CLEAR TI_BDCDATA.
      MOVE: 'BDC_OKCODE'      TO TI_BDCDATA-FNAM,
            '=BU'             TO TI_BDCDATA-FVAL.
      APPEND TI_BDCDATA.

      CLEAR TI_BDCDATA.
      MOVE: 'BSEG-ZTERM'      TO TI_BDCDATA-FNAM,
            ''                TO TI_BDCDATA-FVAL.
      APPEND TI_BDCDATA.

      CLEAR TI_BDCDATA.
      MOVE: 'BSEG-GSBER'      TO TI_BDCDATA-FNAM,
            IT_TOTALIZA-GSBER TO TI_BDCDATA-FVAL.
      APPEND TI_BDCDATA.

      CLEAR TI_BDCDATA.
      MOVE: 'BSEG-SGTXT'      TO TI_BDCDATA-FNAM.
      MESSAGE S039(ZFI) WITH  IT_TOTALIZA-NAME1 INTO TI_BDCDATA-FVAL.
      I = STRLEN( TI_BDCDATA-FVAL ).
      IF I GT 50.
        TI_BDCDATA-FVAL = TI_BDCDATA-FVAL(50).
      ENDIF.
      APPEND TI_BDCDATA.

      CLEAR TI_BDCDATA.
      MOVE: 'BSEG-ZFBDT'      TO TI_BDCDATA-FNAM.
      CONCATENATE IT_TOTALIZA-ZFBDT_RES+6(2)
                  IT_TOTALIZA-ZFBDT_RES+4(2)
                  IT_TOTALIZA-ZFBDT_RES(4) INTO TI_BDCDATA-FVAL.
      APPEND TI_BDCDATA.

    ELSE.
      """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

      "Tela de Documentos - Parcial
      CLEAR TI_BDCDATA.
      MOVE:  'SAPDF05X'       TO TI_BDCDATA-PROGRAM,
             '3100'	          TO TI_BDCDATA-DYNPRO,
             'X'              TO TI_BDCDATA-DYNBEGIN.
      APPEND TI_BDCDATA.

      CLEAR TI_BDCDATA.
      MOVE: 'BDC_OKCODE'      TO TI_BDCDATA-FNAM,
            '=PART'           TO TI_BDCDATA-FVAL.
      APPEND TI_BDCDATA.
      CLEAR TI_BDCDATA.

      "Valor Parcial
      CLEAR TI_BDCDATA.
      MOVE:  'SAPDF05X'       TO TI_BDCDATA-PROGRAM,
             '3100'	          TO TI_BDCDATA-DYNPRO,
             'X'              TO TI_BDCDATA-DYNBEGIN.
      APPEND TI_BDCDATA.


      CLEAR TI_BDCDATA.
      MOVE: 'BDC_OKCODE'      TO TI_BDCDATA-FNAM,
            '/00'             TO TI_BDCDATA-FVAL.
      APPEND TI_BDCDATA.
      CLEAR TI_BDCDATA.

      LC_DOCUMENTO = 1.

      LOOP AT IT_COMPENSAR WHERE DOCUMENTO EQ IT_TOTALIZA-DOCUMENTO..

        CASE IT_TOTALIZA-KOART.
          WHEN 'K'.
            WA_BKPF-DMBTR = ABS( IT_COMPENSAR-DMBTR ) * -1.
          WHEN 'D'.
            WA_BKPF-DMBTR = ABS( IT_COMPENSAR-DMBTR ).
        ENDCASE.

        MOVE LC_DOCUMENTO TO LC_DOC_STR.
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            INPUT  = LC_DOC_STR
          IMPORTING
            OUTPUT = LC_DOC_STR.

        CLEAR TI_BDCDATA.
        CONCATENATE 'DF05B-PSZAH(' LC_DOC_STR ')' INTO TI_BDCDATA-FNAM.
        MOVE:  WA_BKPF-DMBTR    TO TI_BDCDATA-FVAL.
        CONDENSE TI_BDCDATA-FVAL NO-GAPS.
        CALL FUNCTION 'STRING_REPLACE'
          EXPORTING
            PATTERN    = '.'
            SUBSTITUTE = ','
          CHANGING
            TEXT       = TI_BDCDATA-FVAL.
        APPEND TI_BDCDATA.

        ADD 1 TO LC_DOCUMENTO.

      ENDLOOP.

      "Salvar
      CLEAR TI_BDCDATA.
      MOVE:  'SAPDF05X'       TO TI_BDCDATA-PROGRAM,
             '3100'	          TO TI_BDCDATA-DYNPRO,
             'X'              TO TI_BDCDATA-DYNBEGIN.
      APPEND TI_BDCDATA.

      CLEAR TI_BDCDATA.
      MOVE: 'BDC_OKCODE'      TO TI_BDCDATA-FNAM,
            '=BU'             TO TI_BDCDATA-FVAL.
      APPEND TI_BDCDATA.
      CLEAR TI_BDCDATA.

    ENDIF.

    "Execução do SHDB """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    LC_MODE = 'N'.
    " A - Processing with display of screens
    " E - Display of screens only if an error occurs
    " N - Processing without display of screens
    " P - Processing without display of the screens

    IF IT_TOTALIZA-SHKZG EQ 'S' AND IT_TOTALIZA-KOART EQ 'K'.
      "F-28
      CALL TRANSACTION 'F-28' USING TI_BDCDATA MODE LC_MODE UPDATE 'S' MESSAGES INTO IT_MESSAGE.
    ELSEIF IT_TOTALIZA-SHKZG EQ 'H' AND IT_TOTALIZA-KOART EQ 'K'.
      "F-53
      CALL TRANSACTION 'F-53' USING TI_BDCDATA MODE LC_MODE UPDATE 'S' MESSAGES INTO IT_MESSAGE.
    ELSEIF IT_TOTALIZA-SHKZG EQ 'S' AND IT_TOTALIZA-KOART EQ 'D'.
      "F-28
      CALL TRANSACTION 'F-28' USING TI_BDCDATA MODE LC_MODE UPDATE 'S' MESSAGES INTO IT_MESSAGE.
    ELSEIF IT_TOTALIZA-SHKZG EQ 'H' AND IT_TOTALIZA-KOART EQ 'D'.
      "F-53
      CALL TRANSACTION 'F-53' USING TI_BDCDATA MODE LC_MODE UPDATE 'S' MESSAGES INTO IT_MESSAGE.
    ENDIF.
    COMMIT WORK.

    MOVE: 'S'   TO WA_RETORNO-MSGTYP,
          'F5'  TO WA_RETORNO-MSGID,
          '312' TO WA_RETORNO-MSGNR.

    DELETE IT_MESSAGE WHERE MSGID EQ 'F5' AND MSGNR EQ '074'.

    READ TABLE IT_MESSAGE INTO WA_RETORNO
    WITH KEY MSGTYP = WA_RETORNO-MSGTYP
             MSGID  = WA_RETORNO-MSGID
             MSGNR  = WA_RETORNO-MSGNR.
    IF SY-SUBRC IS INITIAL.
      MOVE IT_COMPENSAR TO WA_BKPF.
      WA_BKPF-BELNR_C = WA_RETORNO-MSGV1.
      WA_BKPF-GJAHR_C = IT_COMPENSAR-BUDAT(4).
      APPEND WA_BKPF TO IT_BKPF_RET.
    ENDIF.

    LOOP AT IT_MESSAGE INTO WA_RETORNO.
      APPEND WA_RETORNO TO IT_RETORNO.
    ENDLOOP.
    CLEAR: IT_MESSAGE.
    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

  ENDLOOP.

  DATA: SL_RETURN TYPE BAPIRET2.

  LOOP AT IT_RETORNO.
    SL_RETURN-TYPE       = IT_RETORNO-MSGTYP.
    SL_RETURN-ID         = IT_RETORNO-MSGID.
    SL_RETURN-NUMBER     = IT_RETORNO-MSGNR.
    SL_RETURN-MESSAGE_V1 = IT_RETORNO-MSGV1.
    SL_RETURN-MESSAGE_V2 = IT_RETORNO-MSGV2.
    SL_RETURN-MESSAGE_V3 = IT_RETORNO-MSGV3.
    SL_RETURN-MESSAGE_V4 = IT_RETORNO-MSGV4.

    MESSAGE ID SL_RETURN-ID TYPE SL_RETURN-TYPE
     NUMBER SL_RETURN-NUMBER
       WITH SL_RETURN-MESSAGE_V1 SL_RETURN-MESSAGE_V2 SL_RETURN-MESSAGE_V3 SL_RETURN-MESSAGE_V4
       INTO SL_RETURN-MESSAGE.

    APPEND SL_RETURN TO IT_BKPF_RET2.
    CLEAR: SL_RETURN.
  ENDLOOP.

ENDFUNCTION.
