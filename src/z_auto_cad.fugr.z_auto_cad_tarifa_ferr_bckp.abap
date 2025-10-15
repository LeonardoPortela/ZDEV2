DATA: bdcdata    TYPE STANDARD TABLE OF bdcdata,
      bdcmsgcoll TYPE STANDARD TABLE OF bdcmsgcoll.

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


FUNCTION Z_AUTO_CAD_TARIFA_FERR_BCKP
*"--------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_CONDICAO) TYPE  CHAR1
*"     REFERENCE(I_SHTYP) TYPE  KOMG-SHTYP
*"     REFERENCE(I_TDLNR) TYPE  KOMG-TDLNR
*"     REFERENCE(I_LZONEA) TYPE  KOMG-LZONEA
*"     REFERENCE(I_LZONEZ) TYPE  KOMG-LZONEZ
*"     REFERENCE(I_KBETR) TYPE  KONP-KBETR
*"     REFERENCE(I_MATNR) TYPE  KOMG-MATNR OPTIONAL
*"     REFERENCE(I_PSTLZA) TYPE  PSTLZ OPTIONAL
*"     REFERENCE(I_VSTEL) TYPE  KOMG-VSTEL OPTIONAL
*"     REFERENCE(I_DATAB) TYPE  RV13A-DATAB
*"  EXPORTING
*"     REFERENCE(E_MESSAGE) TYPE  STRING
*"--------------------------------------------------------------------
 " You can use the template 'functionModuleParameter' to add here the signature!
.

  "Tabelas:
  "A910 Condição 1 e 4
  "A938 Condição 2
  "A933 Condição 3


  DATA: p_KBETR    TYPE char13,
        _tarifa    TYPE char13,
        l_msgno    TYPE syst_msgno.

  CLEAR: p_KBETR, _tarifa, l_msgno, bdcdata, bdcmsgcoll. "178025 CS2023000574 Job dinâmico PSA

  IF i_condicao IS NOT INITIAL.
    IF ( i_condicao = 1 OR i_condicao = 4 ) AND ( i_shtyp IS INITIAL OR i_tdlnr IS INITIAL OR i_lzonea IS INITIAL OR i_lzonez IS INITIAL OR i_kbetr IS INITIAL OR i_datab IS INITIAL ).
      MESSAGE w001(zmsg) WITH 'Condição 1 ou 4 não Atendida!'.
    ENDIF.
    IF ( i_condicao = 2 ) AND
      ( i_shtyp IS INITIAL OR i_tdlnr IS INITIAL OR i_lzonea IS INITIAL OR i_lzonez IS INITIAL OR i_kbetr IS INITIAL OR i_datab IS INITIAL OR i_matnr IS INITIAL OR i_pstlza IS INITIAL ).
      MESSAGE w001(zmsg) WITH 'Condição 2 não Atendida!'.
    ENDIF.
    IF ( i_condicao = 3 ) AND
      ( i_shtyp IS INITIAL OR i_tdlnr IS INITIAL OR i_lzonea IS INITIAL OR i_lzonez IS INITIAL OR i_kbetr IS INITIAL OR i_datab IS INITIAL OR i_vstel IS INITIAL ).
      MESSAGE w001(zmsg) WITH 'Condição 3 não Atendida!'.
    ENDIF.
  ELSE.
    MESSAGE w001(zmsg) WITH 'Condição não pode ser vazia ou nula!'.
  ENDIF.

  PERFORM bdc_dynpro      USING 'SAPMV13A'              '0100'.
  PERFORM bdc_field       USING 'BDC_CURSOR'            'RV13A-KSCHL'.
  PERFORM bdc_field       USING 'RV13A-KSCHL'           'ZFRE'.
  PERFORM bdc_field       USING 'BDC_OKCODE'            '/00'.

  PERFORM bdc_dynpro      USING 'SAPLV14A'              '0100'.


  DATA(_datab) = |01.{ i_datab+4(2) }.{ i_datab+0(4) }|.
  DATA(_datab2) =  |{ i_datab+0(4) }{ i_datab+4(2) }01|.
  DATA p_datab TYPE sy-datum.
  p_datab = _datab2.
  p_KBETR = i_KBETR.
  CONDENSE p_KBETR NO-GAPS.
  WRITE i_KBETR TO _tarifa.

  CASE i_condicao.

    WHEN '1'. "2.1 910

      PERFORM bdc_field       USING 'BDC_CURSOR'                    'RV130-SELKZ(05)'.
      PERFORM bdc_field       USING 'RV130-SELKZ(05)'               'X'.
      PERFORM bdc_field       USING 'BDC_OKCODE'                    '/00'.

      PERFORM bdc_dynpro      USING 'SAPMV13A'                      '1910'.
      PERFORM bdc_field       USING 'BDC_CURSOR'                    'RV13A-DATBI(01)'.
      PERFORM bdc_field       USING 'BDC_OKCODE'                    '/00'.
      PERFORM bdc_field       USING 'KOMG-SHTYP'                    i_shtyp."'Z031'.
      PERFORM bdc_field       USING 'KOMG-TDLNR'                    i_TDLNR. "'159775'.
      PERFORM bdc_field       USING 'KOMG-LZONEA'                   i_LZONEA."'TERMAG'.
      PERFORM bdc_field       USING 'KOMG-LZONEZ(01)'               i_LZONEZ."'JMLINKTERM'.
      PERFORM bdc_field       USING 'KONP-KBETR(01)'                _tarifa."'          110,00'.
      PERFORM bdc_field       USING 'KONP-KONWA(01)'                'BRL'.
      PERFORM bdc_field       USING 'KONP-KPEIN(01)'                '    1'.
      PERFORM bdc_field       USING 'KONP-KMEIN(01)'                'TO'.
      PERFORM bdc_field       USING 'RV13A-DATAB(01)'               _datab."'01032025'.
      PERFORM bdc_field       USING 'RV13A-DATBI(01)'               '31129999'.

      PERFORM bdc_dynpro      USING 'SAPMV13A'                      '1910'.
      PERFORM bdc_field       USING 'BDC_CURSOR'                    'KOMG-LZONEZ(01)'.



    WHEN '2'."2.2 938

      PERFORM bdc_field       USING 'BDC_CURSOR'                    'RV130-SELKZ(11)'.
      PERFORM bdc_field       USING 'RV130-SELKZ(11)'               'X'.
      PERFORM bdc_field       USING 'BDC_OKCODE'                    '/00'.

      PERFORM bdc_dynpro      USING 'SAPMV13A'                      '1938'."p_SAPMV13A.
      PERFORM bdc_field       USING 'BDC_CURSOR'                    'RV13A-DATBI(01)'.
      PERFORM bdc_field       USING 'BDC_OKCODE'                    '/00'.
      PERFORM bdc_field       USING 'KOMG-SHTYP'                    i_shtyp."'Z031'.
      PERFORM bdc_field       USING 'KOMG-TDLNR'                    i_TDLNR. "'159775'.
      PERFORM bdc_field       USING 'KOMG-LZONEA'                   i_LZONEA."'TERMAG'.
      PERFORM bdc_field       USING 'KOMG-LZONEZ'               i_LZONEZ."'JMLINKTERM'.
      PERFORM bdc_field       USING 'KONP-KBETR(01)'                _tarifa."'          110,00'.
      PERFORM bdc_field       USING 'KONP-KONWA(01)'                'BRL'.
      PERFORM bdc_field       USING 'RV13A-DATAB(01)'               _datab. "01.mes.ano*
      PERFORM bdc_field       USING 'RV13A-DATBI(01)'               '31129999'."*
      PERFORM bdc_field       USING 'KONP-KPEIN(01)'                '    1'.
      PERFORM bdc_field       USING 'KOMG-MATNR'                    i_matnr."Material
      PERFORM bdc_field       USING 'KOMG-PSTLZA(01)'               i_PSTLZA."Cep
      PERFORM bdc_field       USING 'KONP-KMEIN(01)'                'TO'.

      PERFORM bdc_dynpro      USING 'SAPMV13A'                      '1938'.
      PERFORM bdc_field       USING 'BDC_CURSOR'                    'KOMG-PSTLZA(01)'.

    WHEN '3'."2.3

      PERFORM bdc_field       USING 'BDC_CURSOR'                    'RV130-SELKZ(09)'.
      PERFORM bdc_field       USING 'RV130-SELKZ(09)'               'X'.
      PERFORM bdc_field       USING 'BDC_OKCODE'                    '/00'.

      PERFORM bdc_dynpro      USING 'SAPMV13A'                      '1933'.
      PERFORM bdc_field       USING 'BDC_CURSOR'                    'RV130-SELKZ(09)'.
      PERFORM bdc_field       USING 'BDC_OKCODE'                    '/00'.
      PERFORM bdc_field       USING 'KOMG-SHTYP'                    i_shtyp. "Tipo de Frete
      PERFORM bdc_field       USING 'KOMG-TDLNR'                    i_TDLNR. "Agente de Frete
      PERFORM bdc_field       USING 'KOMG-LZONEA'                   i_LZONEA. "Zona de Origem
      PERFORM bdc_field       USING 'KOMG-LZONEZ'               i_LZONEZ."Zona de Chega
      PERFORM bdc_field       USING 'KONP-KBETR(01)'                _tarifa."'          110,00'.
      PERFORM bdc_field       USING 'KONP-KONWA(01)'                'BRL'.
      PERFORM bdc_field       USING 'RV13A-DATAB(01)'               _datab. "01.mes.ano*
      PERFORM bdc_field       USING 'RV13A-DATBI(01)'               '31129999'."*
      PERFORM bdc_field       USING 'KONP-KPEIN(01)'                '    1'.
      PERFORM bdc_field       USING 'KONP-KMEIN(01)'                'TO'.
      PERFORM bdc_field       USING 'KOMG-VSTEL(01)'                i_VSTEL.

      PERFORM bdc_dynpro      USING 'SAPMV13A'                      '1933'.
      PERFORM bdc_field       USING 'BDC_CURSOR'                    'KOMG-VSTEL(01)'.

    WHEN '4'. "910 Lotação

      PERFORM bdc_field       USING 'BDC_CURSOR'                    'RV130-SELKZ(05)'.
      PERFORM bdc_field       USING 'RV130-SELKZ(05)'               'X'.
      PERFORM bdc_field       USING 'BDC_OKCODE'                    '/00'.

      PERFORM bdc_dynpro      USING 'SAPMV13A'                      '1910'.
      PERFORM bdc_field       USING 'BDC_CURSOR'                    'RV13A-DATBI(01)'.
      PERFORM bdc_field       USING 'BDC_OKCODE'                    '/00'.
      PERFORM bdc_field       USING 'KOMG-SHTYP'                    i_shtyp."'Z031'.
      PERFORM bdc_field       USING 'KOMG-TDLNR'                    i_TDLNR. "'159775'.
      PERFORM bdc_field       USING 'KOMG-LZONEA'                   i_LZONEA."'TERMAG'.
      PERFORM bdc_field       USING 'KOMG-LZONEZ(01)'               i_LZONEZ."'JMLINKTERM'.
      PERFORM bdc_field       USING 'KONP-KBETR(01)'                _tarifa."'          110,00'.
      PERFORM bdc_field       USING 'KONP-KONWA(01)'                'BRL'.
      PERFORM bdc_field       USING 'RV13A-DATAB(01)'               _datab."'01032025'.
      PERFORM bdc_field       USING 'RV13A-DATBI(01)'               '31129999'.
      PERFORM bdc_field       USING 'RV13A-KRECH(01)'               'B'.

      PERFORM bdc_dynpro      USING 'SAPMV13A'                      '1910'.
      PERFORM bdc_field       USING 'BDC_CURSOR'                    'KOMG-LZONEZ(01)'.

  ENDCASE.

  PERFORM bdc_field       USING 'BDC_OKCODE'            '=SICH' .

  DATA: _mode   TYPE char1,
        _update TYPE char1.

  CLEAR: _mode,_update.
  _mode = |N|.
  _update = |S|.

  CALL TRANSACTION 'TK11' USING bdcdata MODE _mode UPDATE _update MESSAGES INTO bdcmsgcoll.

  " Check messages for errors
  IF sy-subrc = 0.
    e_message = |SUCESSO|.
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
*      CASE i_condicao.
*        WHEN 1.
*
*          SELECT a~*,b~kbetr FROM a910 AS a
*          INNER JOIN konp AS b ON a~knumh = b~knumh
*          WHERE 1 = 1
*          AND a~shtyp = @i_shtyp
*          AND a~tdlnr = @i_TDLNR
*          AND a~lzonea = @i_LZONEA
*          AND a~lzonez = @i_LZONEZ
*          AND a~datab = @p_datab
*          AND a~datbi = '99991231'
*          AND b~konwa = 'BRL'
*          AND b~kbetr = @i_kbetr
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
*      AND a~shtyp = @i_shtyp
*      AND a~tdlnr = @i_TDLNR
*      AND a~lzonea = @i_LZONEA
*      AND a~lzonez = @i_LZONEZ
*      AND a~datab = @p_datab
*      AND a~datbi = '99991231'
*      AND b~konwa = 'BRL'
*      AND b~kbetr = @i_kbetr
*            AND b~kmein = 'TO'
*            AND b~kpein = '1'
*            AND a~matnr = @i_matnr
*            AND a~pstlza = @i_pstlza
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
*      AND a~shtyp = @i_shtyp
*      AND a~tdlnr = @i_TDLNR
*      AND a~lzonea = @i_LZONEA
*      AND a~lzonez = @i_LZONEZ
*      AND a~datab = @p_datab
*      AND a~datbi = '99991231'
*      AND b~konwa = 'BRL'
*      AND b~kbetr = @i_kbetr
*            AND b~kmein = 'TO'
*            AND b~kpein = '1'
*            AND a~vstel = @i_vstel
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
*      AND a~shtyp = @i_shtyp
*      AND a~tdlnr = @i_TDLNR
*      AND a~lzonea = @i_LZONEA
*      AND a~lzonez = @i_LZONEZ
*      AND a~datab = @p_datab
*      AND a~datbi = '99991231'
*      AND b~krech = 'B'
*      AND b~konwa = 'BRL'
*      AND b~kbetr = @i_kbetr
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
          message_text_output = e_message.
    ENDLOOP.
    ROLLBACK WORK. " Roll back changes if errors occur
  ENDIF.

ENDFUNCTION.
