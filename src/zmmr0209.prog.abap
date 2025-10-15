*&---------------------------------------------------------------------*
*& Report ZMMR0209
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zmmr0209.

TABLES:komg,konp,rv13a.

SELECTION-SCREEN BEGIN OF BLOCK part1 WITH FRAME TITLE TEXT-c01 .

  PARAMETERS: p_cond   TYPE boole NO-DISPLAY,
              p_SHTYP  TYPE komg-shtyp,
              p_TDLNR  TYPE komg-tdlnr,
              p_LZONEA TYPE komg-lzonea,
              p_LZONEZ TYPE komg-lzonez,
              p_KBETR  TYPE konp-kbetr,
              p_MATNR  TYPE komg-matnr,
              p_VSTEL  TYPE komg-vstel,
              p_PSTLZA TYPE komg-pstlza,
              p_DATAB  TYPE rv13a-datab.


SELECTION-SCREEN END OF BLOCK part1.

SELECTION-SCREEN BEGIN OF BLOCK part2 WITH FRAME TITLE TEXT-c02 .

  PARAMETERS: r1 RADIOBUTTON GROUP tb,
              r2 RADIOBUTTON GROUP tb,
              r3 RADIOBUTTON GROUP tb,
              r4 RADIOBUTTON GROUP tb.

SELECTION-SCREEN END OF BLOCK part2.


DATA: bdcdata    TYPE STANDARD TABLE OF bdcdata,
      bdcmsgcoll TYPE STANDARD TABLE OF bdcmsgcoll,
      ls_message TYPE string.


START-OF-SELECTION.

  PERFORM z_auto_cad_tarifa_ferr CHANGING ls_message.


FORM bdc_dynpro USING _program _dynpro.
  DATA: bdc_line TYPE bdcdata.
  CLEAR bdc_line.
  bdc_line-program  = _program.
  bdc_line-dynpro   = _dynpro.
  bdc_line-dynbegin = abap_true.
  APPEND bdc_line TO bdcdata.
  CLEAR bdc_line.
ENDFORM.

FORM bdc_field USING fnam fval.
  DATA: bdc_line TYPE bdcdata.
  CLEAR bdc_line.
  bdc_line-fnam = fnam.
  bdc_line-fval = fval.
  APPEND bdc_line TO bdcdata.
  CLEAR bdc_line.
ENDFORM.


FORM z_auto_cad_tarifa_ferr CHANGING ls_message TYPE string.

  DATA:
    _kbetr  TYPE char15, "konp-kbetr,
    _shtyp  TYPE komg-shtyp,
    _tdlnr  TYPE komg-tdlnr,
    _lzonea TYPE komg-lzonea,
    _lzonez TYPE komg-lzonez,
    _matnr  TYPE komg-matnr,
    _vstel  TYPE komg-vstel,
    _pstlza TYPE komg-pstlza,
    _datab  TYPE char10, "rv13a-datab,
    l_msgno TYPE syst_msgno.

  CLEAR:_kbetr  ,_shtyp  ,_tdlnr  ,_lzonea ,_lzonez ,_matnr  ,_vstel  ,_pstlza ,_datab, l_msgno, bdcdata, bdcmsgcoll. "178025 CS2023000574 Job dinâmico PSA

  TYPES: BEGIN OF ty_zmmr0209,
           condicao TYPE char1,
           shtyp    TYPE komg-shtyp,
           tdlnr    TYPE komg-tdlnr,
           lzonea   TYPE komg-lzonea,
           lzonez   TYPE komg-lzonez,
           kbetr    TYPE konp-kbetr,
           matnr    TYPE komg-matnr,
           pstlza   TYPE pstlz,
           vstel    TYPE komg-vstel,
           datab    TYPE rv13a-datab,
         END OF ty_zmmr0209.

  DATA: ls_zmmr0209 TYPE ty_zmmr0209.

  CLEAR: ls_zmmr0209.

  IMPORT ls_zmmr0209 = ls_zmmr0209 FROM MEMORY ID 'MEMORY_ZMMR0209'.

  IF ls_zmmr0209 IS NOT INITIAL.

    CLEAR: r1,r2,r3,r4.

    CASE ls_zmmr0209-condicao.
      WHEN '1'.
        r1 = abap_true.
      WHEN '2'.
        r2 = abap_true.
      WHEN '3'.
        r3 = abap_true.
      WHEN '4'.
        r4 = abap_true.
    ENDCASE.

    P_kbetr  = ls_zmmr0209-kbetr.
    P_shtyp  = ls_zmmr0209-shtyp.
    P_tdlnr  = ls_zmmr0209-tdlnr.
    P_lzonea = ls_zmmr0209-lzonea.
    P_lzonez = ls_zmmr0209-lzonez.
    P_matnr  = ls_zmmr0209-matnr.
    P_vstel  = ls_zmmr0209-vstel.
    P_pstlza = ls_zmmr0209-pstlza.
    P_datab = ls_zmmr0209-datab.
  ENDIF.

  _datab = |01.{ p_datab+4(2) }.{ p_datab+0(4) }|.
  CONDENSE _datab NO-GAPS.

  WRITE p_KBETR TO _kbetr.
  CONDENSE _kbetr NO-GAPS.

  _shtyp  = p_shtyp.
  _tdlnr  = p_tdlnr.
  _lzonea = p_lzonea.
  _lzonez = p_lzonez.
  _matnr  = p_matnr.
  _vstel  = p_vstel.
  _pstlza = p_pstlza.

  "A910
  IF ( r1 = abap_true OR r4 = abap_true )
    AND
    ( _shtyp IS INITIAL OR _tdlnr IS INITIAL OR _lzonea IS INITIAL OR _lzonez IS INITIAL OR _kbetr IS INITIAL OR _datab IS INITIAL ).
    MESSAGE w001(zmsg) WITH 'Condição 1 ou 4 não Atendida!'.
  ENDIF.
  "A938
  IF ( r2 = abap_true )
     AND
    ( _shtyp IS INITIAL OR _tdlnr IS INITIAL OR _lzonea IS INITIAL OR _lzonez IS INITIAL OR _kbetr IS INITIAL OR _datab IS INITIAL OR _matnr IS INITIAL OR _pstlza IS INITIAL ).
    MESSAGE w001(zmsg) WITH 'Condição 2 não Atendida!'.
  ENDIF.

  "A933
  IF ( r3 = abap_true )
    AND
    ( _shtyp IS INITIAL OR _tdlnr IS INITIAL OR _lzonea IS INITIAL OR _lzonez IS INITIAL OR _kbetr IS INITIAL OR _datab IS INITIAL OR _vstel IS INITIAL ).
    MESSAGE w001(zmsg) WITH 'Condição 3 não Atendida!'.
  ENDIF.

  PERFORM bdc_dynpro      USING 'SAPMV13A'              '0100'.
  PERFORM bdc_field       USING 'BDC_CURSOR'            'RV13A-KSCHL'.
  PERFORM bdc_field       USING 'RV13A-KSCHL'           'ZFRE'.
  PERFORM bdc_field       USING 'BDC_OKCODE'            '/00'.

  PERFORM bdc_dynpro      USING 'SAPLV14A'              '0100'.

  "Tabelas:
  "A910 Condição 1 e 4
  "A938 Condição 2
  "A933 Condição 3

  IF r1 = abap_true. "A910
    "2.1 910

    PERFORM bdc_field       USING 'BDC_CURSOR'                    'RV130-SELKZ(05)'.
    PERFORM bdc_field       USING 'RV130-SELKZ(05)'               'X'.
    PERFORM bdc_field       USING 'BDC_OKCODE'                    '/00'.

    PERFORM bdc_dynpro      USING 'SAPMV13A'                      '1910'.
    PERFORM bdc_field       USING 'BDC_CURSOR'                    'RV13A-DATBI(01)'.
    PERFORM bdc_field       USING 'BDC_OKCODE'                    '/00'.
    PERFORM bdc_field       USING 'KOMG-SHTYP'                    _shtyp."'Z031'.
    PERFORM bdc_field       USING 'KOMG-TDLNR'                    _tdlnr. "'159775'.
    PERFORM bdc_field       USING 'KOMG-LZONEA'                   _lzonea."'TERMAG'.
    PERFORM bdc_field       USING 'KOMG-LZONEZ(01)'               _lzonez."'JMLINKTERM'.
    PERFORM bdc_field       USING 'KONP-KBETR(01)'                _kbetr."'          110,00'.
    PERFORM bdc_field       USING 'KONP-KONWA(01)'                'BRL'.
    PERFORM bdc_field       USING 'KONP-KPEIN(01)'                '    1'.
    PERFORM bdc_field       USING 'KONP-KMEIN(01)'                'TO'.
    PERFORM bdc_field       USING 'RV13A-DATAB(01)'               _datab."'01032025'.
    PERFORM bdc_field       USING 'RV13A-DATBI(01)'               '31129999'.

    PERFORM bdc_dynpro      USING 'SAPMV13A'                      '1910'.
    PERFORM bdc_field       USING 'BDC_CURSOR'                    'KOMG-LZONEZ(01)'.

  ENDIF.

  IF r2 = abap_true. "A938

    PERFORM bdc_field       USING 'BDC_CURSOR'                    'RV130-SELKZ(11)'.
    PERFORM bdc_field       USING 'RV130-SELKZ(11)'               'X'.
    PERFORM bdc_field       USING 'BDC_OKCODE'                    '/00'.

    PERFORM bdc_dynpro      USING 'SAPMV13A'                      '1938'."p_SAPMV13A.
    PERFORM bdc_field       USING 'BDC_CURSOR'                    'RV13A-DATBI(01)'.
    PERFORM bdc_field       USING 'BDC_OKCODE'                    '/00'.
    PERFORM bdc_field       USING 'KOMG-SHTYP'                    _shtyp."'Z031'.
    PERFORM bdc_field       USING 'KOMG-TDLNR'                    _tdlnr. "'159775'.
    PERFORM bdc_field       USING 'KOMG-LZONEA'                   _lzonea."'TERMAG'.
    PERFORM bdc_field       USING 'KOMG-LZONEZ'                   _lzonez."'JMLINKTERM'.
    PERFORM bdc_field       USING 'KONP-KBETR(01)'                _kbetr."'          110,00'.
    PERFORM bdc_field       USING 'KONP-KONWA(01)'                'BRL'.
    PERFORM bdc_field       USING 'RV13A-DATAB(01)'               _datab. "01.mes.ano*
    PERFORM bdc_field       USING 'RV13A-DATBI(01)'               '31129999'."*
    PERFORM bdc_field       USING 'KONP-KPEIN(01)'                '    1'.
    PERFORM bdc_field       USING 'KOMG-MATNR'                    _matnr."Material
    PERFORM bdc_field       USING 'KOMG-PSTLZA(01)'               _pstlza."Cep
    PERFORM bdc_field       USING 'KONP-KMEIN(01)'                'TO'.

    PERFORM bdc_dynpro      USING 'SAPMV13A'                      '1938'.
    PERFORM bdc_field       USING 'BDC_CURSOR'                    'KOMG-PSTLZA(01)'.
  ENDIF.

  IF r3 = abap_true. "A933

    PERFORM bdc_field       USING 'BDC_CURSOR'                    'RV130-SELKZ(09)'.
    PERFORM bdc_field       USING 'RV130-SELKZ(09)'               'X'.
    PERFORM bdc_field       USING 'BDC_OKCODE'                    '/00'.

    PERFORM bdc_dynpro      USING 'SAPMV13A'                      '1933'.
    PERFORM bdc_field       USING 'BDC_CURSOR'                    'RV130-SELKZ(09)'.
    PERFORM bdc_field       USING 'BDC_OKCODE'                    '/00'.
    PERFORM bdc_field       USING 'KOMG-SHTYP'                    _shtyp. "Tipo de Frete
    PERFORM bdc_field       USING 'KOMG-TDLNR'                    _tdlnr. "Agente de Frete
    PERFORM bdc_field       USING 'KOMG-LZONEA'                   _lzonea. "Zona de Origem
    PERFORM bdc_field       USING 'KOMG-LZONEZ'                   _lzonez."Zona de Chega
    PERFORM bdc_field       USING 'KONP-KBETR(01)'                _kbetr."'          110,00'.
    PERFORM bdc_field       USING 'KONP-KONWA(01)'                'BRL'.
    PERFORM bdc_field       USING 'RV13A-DATAB(01)'               _datab. "01.mes.ano*
    PERFORM bdc_field       USING 'RV13A-DATBI(01)'               '31129999'."*
    PERFORM bdc_field       USING 'KONP-KPEIN(01)'                '    1'.
    PERFORM bdc_field       USING 'KONP-KMEIN(01)'                'TO'.
    PERFORM bdc_field       USING 'KOMG-VSTEL(01)'                p_VSTEL.

    PERFORM bdc_dynpro      USING 'SAPMV13A'                      '1933'.
    PERFORM bdc_field       USING 'BDC_CURSOR'                    'KOMG-VSTEL(01)'.
  ENDIF.

  IF r4 = abap_true. "A910

    PERFORM bdc_field       USING 'BDC_CURSOR'                    'RV130-SELKZ(05)'.
    PERFORM bdc_field       USING 'RV130-SELKZ(05)'               'X'.
    PERFORM bdc_field       USING 'BDC_OKCODE'                    '/00'.

    PERFORM bdc_dynpro      USING 'SAPMV13A'                      '1910'.
    PERFORM bdc_field       USING 'BDC_CURSOR'                    'RV13A-DATBI(01)'.
    PERFORM bdc_field       USING 'BDC_OKCODE'                    '/00'.
    PERFORM bdc_field       USING 'KOMG-SHTYP'                    _shtyp."'Z031'.
    PERFORM bdc_field       USING 'KOMG-TDLNR'                    _tdlnr. "'159775'.
    PERFORM bdc_field       USING 'KOMG-LZONEA'                   _lzonea."'TERMAG'.
    PERFORM bdc_field       USING 'KOMG-LZONEZ(01)'               _lzonez."'JMLINKTERM'.
    PERFORM bdc_field       USING 'KONP-KBETR(01)'                _kbetr."'          110,00'.
    PERFORM bdc_field       USING 'KONP-KONWA(01)'                'BRL'.
    PERFORM bdc_field       USING 'RV13A-DATAB(01)'               _datab."'01032025'.
    PERFORM bdc_field       USING 'RV13A-DATBI(01)'               '31129999'.
    PERFORM bdc_field       USING 'RV13A-KRECH(01)'               'B'.

    PERFORM bdc_dynpro      USING 'SAPMV13A'                      '1910'.
    PERFORM bdc_field       USING 'BDC_CURSOR'                    'KOMG-LZONEZ(01)'.
  ENDIF.

  PERFORM bdc_field       USING 'BDC_OKCODE'            '=SICH' .

  DATA: _mode   TYPE char1,
        _update TYPE char1.

  CLEAR: _mode,_update.
  _mode = |N|.
  _update = |S|.

  CALL TRANSACTION 'TK11' USING bdcdata MODE _mode UPDATE _update MESSAGES INTO bdcmsgcoll.

  " Check messages for errors
  IF sy-subrc = 0.
    ls_message = |SUCESSO|.
    COMMIT WORK AND WAIT.

    "Tabelas:
    "A910 Condição 1 e 4
    "A938 Condição 2
    "A933 Condição 3

*    DATA: qtd         TYPE i VALUE 30,
*          _t          TYPE i,
*          lv_finished TYPE boolean.
*
*    FIELD-SYMBOLS: <fs> TYPE ANY TABLE.
*
*    lv_finished = abap_false.
*    DO 5 TIMES.
*      _t = _t + 1.
*
*      CASE p_condicao.
*        WHEN 1.
*
*          SELECT a~*,b~kbetr FROM a910 AS a
*          INNER JOIN konp AS b ON a~knumh = b~knumh
*          WHERE 1 = 1
*          AND a~shtyp = @p_shtyp
*          AND a~tdlnr = @p_TDLNR
*          AND a~lzonea = @p_LZONEA
*          AND a~lzonez = @p_LZONEZ
*          AND a~datab = @p_datab
*          AND a~datbi = '99991231'
*          AND b~konwa = 'BRL'
*          AND b~kbetr = @p_kbetr
*          AND b~kmein = 'TO'
*          AND b~kpein = '1'
*          INTO TABLE @DATA(lt_a910_c1).
*
*          ASSIGN lt_a910_c1 TO <fs>.
*
*        WHEN 2.
*
*          SELECT a~*,b~kbetr FROM a938 AS a
*      INNER JOIN konp AS b ON a~knumh = b~knumh
*      WHERE 1 = 1
*      AND a~shtyp = @p_shtyp
*      AND a~tdlnr = @p_TDLNR
*      AND a~lzonea = @p_LZONEA
*      AND a~lzonez = @p_LZONEZ
*      AND a~datab = @p_datab
*      AND a~datbi = '99991231'
*      AND b~konwa = 'BRL'
*      AND b~kbetr = @p_kbetr
*            AND b~kmein = 'TO'
*            AND b~kpein = '1'
*            AND a~matnr = @p_matnr
*            AND a~pstlza = @p_pstlza
*            INTO TABLE @DATA(lt_a938_c2).
*
*          ASSIGN lt_a938_c2 TO <fs>.
*
*        WHEN 3.
*
*          SELECT a~*,b~kbetr FROM a933 AS a
*      INNER JOIN konp AS b ON a~knumh = b~knumh
*           " INNER JOIN KOMG as c on a~knumh = c~kn
*      WHERE 1 = 1
*      AND a~shtyp = @p_shtyp
*      AND a~tdlnr = @p_TDLNR
*      AND a~lzonea = @p_LZONEA
*      AND a~lzonez = @p_LZONEZ
*      AND a~datab = @p_datab
*      AND a~datbi = '99991231'
*      AND b~konwa = 'BRL'
*      AND b~kbetr = @p_kbetr
*            AND b~kmein = 'TO'
*            AND b~kpein = '1'
*            AND a~vstel = @p_vstel
*            INTO TABLE @DATA(lt_a933_c3).
*
*          ASSIGN lt_a933_c3 TO <fs>.
*
*        WHEN 4.
*
*          SELECT a~*,b~kbetr FROM a910 AS a
*      INNER JOIN konp AS b ON a~knumh = b~knumh
*      WHERE 1 = 1
*      AND a~kschl = 'ZFRE'
*      AND a~shtyp = @p_shtyp
*      AND a~tdlnr = @p_TDLNR
*      AND a~lzonea = @p_LZONEA
*      AND a~lzonez = @p_LZONEZ
*      AND a~datab = @p_datab
*      AND a~datbi = '99991231'
*      AND b~krech = 'B'
*      AND b~konwa = 'BRL'
*      AND b~kbetr = @p_kbetr
*            INTO TABLE @DATA(lt_a910_c4).
*
*          ASSIGN lt_a910_c4 TO <fs>.
*
*      ENDCASE.
*
*      IF <fs> IS NOT INITIAL.
*        lv_finished = abap_true.
*        EXIT.
*      ELSE.
*        IF _t >= 5.
*          EXIT.
*        ELSE.
*          WAIT UP TO 3 SECONDS.
*        ENDIF.
*      ENDIF.
*    ENDDO.

  ELSE.
    " Handle errors from bdcmsgcoll
    LOOP AT bdcmsgcoll INTO DATA(ls_msg) WHERE msgtyp = 'E' OR msgtyp = 'A'.
      CALL FUNCTION 'MESSAGE_TEXT_BUILD'
        EXPORTING
          msgid               = ls_msg-msgid
          msgnr               = ls_msg-msgnr
          msgv1               = ls_msg-msgv1
          msgv2               = ls_msg-msgv2
          msgv3               = ls_msg-msgv3
          msgv4               = ls_msg-msgv4
        IMPORTING
          message_text_output = ls_message.
    ENDLOOP.
    ROLLBACK WORK. " Roll back changes if errors occur
  ENDIF.

  FREE MEMORY ID 'MEMORY_ZMMR0209'.

ENDFORM.
