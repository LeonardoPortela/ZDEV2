************************************************************************
*     P R O J E T O  C R E S C E R   -   M A G G I                     *
*                                                                      *
************************************************************************
* Consultoria ...: Braxis It Services                                  *
* Responsável ...: Geraldo Márcio Santos de Santana - Consultor ABAP   *
* Data desenv ...: 04.06.2007                                          *
* Tipo de prg ...: Enchacement                                         *
* Objetivo    ...: Alteração motivo de pagamento no arq de retorno do  *
*                  Bradesco.                                           *
*                                                                      *
************************************************************************
* Data Modif    Autor                Descriçao            Request      *
************************************************************************
* 04.06.2007    Geraldo M S Santana  First Code                        *
*                                                                      *
*                                                                      *
*                                                                      *
*                                                                      *
************************************************************************
*
*
*&---------------------------------------------------------------------*
*&  Include           ZXDTAU01
*&---------------------------------------------------------------------*

TABLES reguh.

DATA:
  BEGIN OF wa_regup,
    belnr LIKE regup-belnr,
    xref2 LIKE regup-xref2,
    mandt LIKE regup-mandt,
    laufd LIKE regup-laufd,
    laufi LIKE regup-laufi,
    xvorl LIKE regup-xvorl,
    zbukr LIKE regup-zbukr,
    lifnr LIKE regup-lifnr,
    kunnr LIKE regup-kunnr,
    empfg LIKE regup-empfg,
    vblnr LIKE regup-vblnr,
    bukrs LIKE regup-bukrs,
    gjahr LIKE regup-gjahr,
    buzei LIKE regup-buzei,
    zlsch LIKE regup-zlsch,
  END   OF wa_regup.

DATA: wa_arqtxt(500)   TYPE c,
      wa_arqitau3(400) TYPE c,
      wa_arqhsbc(240)  TYPE c,
      wa_aux(240)      TYPE c,

      wa_regut         LIKE regut,
      it_arqtxt        LIKE STANDARD TABLE OF wa_arqtxt,
      it_arqhsbc       LIKE STANDARD TABLE OF wa_arqhsbc,
      it_arqitau3      LIKE STANDARD TABLE OF wa_arqitau3,
      it_regup         LIKE STANDARD TABLE OF wa_regup,
      vl_cami          TYPE string,
      vl_belnr         LIKE bkpf-belnr,
      vl_vblnr         LIKE reguh-vblnr,
      vl_conta(14)     TYPE c,
      vl_first         TYPE c,
      vl_write_lf      TYPE c,
      vl_write_eol     TYPE c,
      vl_zlsch         TYPE regup-zlsch.


FIELD-SYMBOLS: <fs_arqtxt> TYPE table.
*
* Carrega arquivo texto com dados do banco Bradesco
*
break abap.
REFRESH: it_arqtxt, it_arqhsbc.
vl_cami = i_filename.
READ TABLE t_regut INTO wa_regut INDEX 1.
SELECT SINGLE *
      FROM reguh
     WHERE ( laufd EQ wa_regut-laufd )
       AND ( laufi EQ wa_regut-laufi )
       AND ( xvorl EQ wa_regut-xvorl )
       AND ( zbukr EQ wa_regut-zbukr )
       AND ( vblnr NE space       OR
             vblnr IS NOT NULL       ).

IF reguh-rzawe = 'H' .
  EXIT. "folha não modifica o arquivo
ENDIF.

*> Valida se o banco é o Bradesco

READ TABLE t_regut INTO wa_regut INDEX 1.
IF ( wa_regut-dtkey+00(03) EQ 'BBD' ).
  ASSIGN ('IT_ARQTXT[]')  TO <fs_arqtxt>.
ELSE.
  IF wa_regut-dtkey+00(06) = 'ITAU3'.
*** US - 81184 - Inicio CBRAND
    EXIT.
    "ASSIGN ('IT_ARQITAU3[]') TO <fs_arqtxt>.
  ELSE.
    ASSIGN ('IT_ARQHSBC[]') TO <fs_arqtxt>.
  ENDIF.
ENDIF.


CALL FUNCTION 'GUI_UPLOAD'
  EXPORTING
    filename = vl_cami
  TABLES
    data_tab = <fs_arqtxt>
  EXCEPTIONS
    OTHERS   = 01.

IF ( sy-subrc NE 0 ).
  MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  EXIT.
ENDIF.

*> Processamento do arquivo do ITAU - ALRS CH. 128100
*IF ( WA_REGUT-DTKEY+00(04) EQ 'ITAU' ).
*  READ TABLE IT_ARQHSBC INTO WA_ARQHSBC INDEX 3.
*  VL_VBLNR = WA_ARQHSBC+73(10).
*
*  SELECT SINGLE *
*      FROM REGUH
*      WHERE ( LAUFD EQ WA_REGUT-LAUFD )
*       AND ( LAUFI EQ WA_REGUT-LAUFI )
*       AND ( XVORL EQ WA_REGUT-XVORL )
*       AND ( ZBUKR EQ WA_REGUT-ZBUKR )
*       AND ( VBLNR EQ VL_VBLNR ).
*
*  LOOP AT IT_ARQHSBC INTO WA_ARQHSBC.
*    IF SY-TABIX = 1. "Header 1
*      WA_ARQHSBC+57(01) = ' '.
*      WA_ARQHSBC+71(01) = WA_ARQHSBC+70(01).
*      WA_ARQHSBC+70(01) = ' '.
*      MODIFY IT_ARQHSBC FROM WA_ARQHSBC.
*    ELSEIF SY-TABIX = 2. "Header 2
*      IF REGUH-RZAWE = 'M'.
*        WA_ARQHSBC+11(02) = '03'.
*      ENDIF.
*      WA_ARQHSBC+13(03) = '040'.
*      WA_ARQHSBC+57(01) = ' '.
*      WA_ARQHSBC+71(01) = WA_ARQHSBC+70(01).
*      WA_ARQHSBC+70(01) = ' '.
*      MODIFY IT_ARQHSBC FROM WA_ARQHSBC.
*    ELSE.
*      EXIT.
*    ENDIF.
*  ENDLOOP.
*ENDIF.

*> Processamento do arquivo do SICRED - ALRS
IF ( wa_regut-dtkey+00(05) EQ 'SIC-C' ).
  LOOP AT it_arqhsbc INTO wa_arqhsbc.
    IF sy-tabix = 1 OR sy-tabix = 2. "Header 2. "Header 1
      wa_arqhsbc+71(01) = '9'.
      IF  sy-tabix = 2.
        SELECT SINGLE *
           FROM reguh
          WHERE ( laufd EQ wa_regut-laufd )
            AND ( laufi EQ wa_regut-laufi )
            AND ( xvorl EQ wa_regut-xvorl )
            AND ( zbukr EQ wa_regut-zbukr )
            AND ( vblnr NE space       OR
                  vblnr IS NOT NULL       ).
*      01 Crédito em Conta-Corrente   ‘U’
*      03 DOC                                       ‘M’
*      41 TED – Transferência entre Clientes  ‘S’
        IF reguh-rzawe = 'U'.
          wa_arqhsbc+11(02) = '01'.
        ELSEIF reguh-rzawe = 'M'.
          wa_arqhsbc+11(02) = '03'.
        ELSEIF reguh-rzawe = 'S'.
          wa_arqhsbc+11(02) = '41'.
        ENDIF.
      ENDIF.
      MODIFY it_arqhsbc FROM wa_arqhsbc.
    ELSEIF sy-tabix = 3.
      SELECT SINGLE *
         FROM reguh
        WHERE ( laufd EQ wa_regut-laufd )
          AND ( laufi EQ wa_regut-laufi )
          AND ( xvorl EQ wa_regut-xvorl )
          AND ( zbukr EQ wa_regut-zbukr )
          AND ( vblnr NE space       OR
                vblnr IS NOT NULL       ).
*01 Crédito em Conta-Corrente   ‘U’
*03 DOC                                       ‘M’
*41 TED – Transferência entre Clientes  ‘S’
      IF reguh-rzawe = 'U'.
        wa_arqhsbc+11(02) = '01'.
      ELSEIF reguh-rzawe = 'M'.
        wa_arqhsbc+11(02) = '03'.
      ELSEIF reguh-rzawe = 'S'.
        wa_arqhsbc+11(02) = '41'.
      ENDIF.
      MODIFY it_arqhsbc FROM wa_arqhsbc.
    ELSE.
      EXIT.
    ENDIF.
  ENDLOOP.
ENDIF.

*> Processamento do arquivo do BBRA - ALRS CH. 128100
IF ( wa_regut-dtkey+00(04) EQ 'BBRA' ).
  SELECT SINGLE *
  FROM reguh
 WHERE ( laufd EQ wa_regut-laufd )
   AND ( laufi EQ wa_regut-laufi )
   AND ( xvorl EQ wa_regut-xvorl )
   AND ( zbukr EQ wa_regut-zbukr )
   AND ( vblnr NE '' ).
  LOOP AT it_arqhsbc INTO wa_arqhsbc.
    IF sy-tabix = 1. "Header 1

    ELSEIF sy-tabix = 2. "Header 2
      IF reguh-rzawe = 'T'.
        wa_arqhsbc+11(02) = '05'.
        MODIFY it_arqhsbc FROM wa_arqhsbc.
      ENDIF.
    ELSE.
      EXIT.
    ENDIF.
  ENDLOOP.
ENDIF.


*> Processamento do arquivo do Bradesco
IF ( wa_regut-dtkey+00(03) EQ 'BBD' ).
*> Seleciona meio de pagamento na REGUH através dos dados da proposta
*> contidos na T_REGUT.
  SELECT SINGLE *
    FROM reguh
   WHERE ( laufd EQ wa_regut-laufd )
     AND ( laufi EQ wa_regut-laufi )
     AND ( xvorl EQ wa_regut-xvorl )
     AND ( zbukr EQ wa_regut-zbukr )
     AND ( vblnr NE space       OR
           vblnr IS NOT NULL       ).

  LOOP AT it_arqtxt INTO wa_arqtxt.
    CHECK ( wa_arqtxt+00(01) EQ '1' ).

    IF ( reguh-rzawe EQ 'M' ).
      wa_arqtxt+263(02) = '03'.
    ELSEIF ( reguh-rzawe EQ 'U' ).
      wa_arqtxt+263(02) = '05'.
    ELSEIF ( 'ST' CS reguh-rzawe ).
      wa_arqtxt+263(02) = '08'.
    ELSEIF ( reguh-rzawe EQ 'E' ).
      wa_arqtxt+263(02) = '31'.
    ELSE.
      wa_arqtxt+263(02) = '00'.
    ENDIF.

    wa_arqtxt+276(01) = '0'.
    wa_arqtxt+277(01) = '1'.
*>    WA_ARQTXT+478(01) = ''.
*******************************************
*    IF  WA_REGUT-ZBUKR EQ '0038'
*                AND  REGUH-RZAWE EQ 'U'.
*      WA_ARQTXT+478(01) = SPACE.
*      WA_ARQTXT+480(7) = REGUH-UBKNT.
*    ENDIF.
*
*    IF WA_REGUT-ZBUKR EQ '0038'
*                    AND REGUH-RZAWE EQ 'S'.
*      WA_ARQTXT+478(01) = SPACE.
*      WA_ARQTXT+480(7) = REGUH-UBKNT.
*    ENDIF.
*
*    IF WA_REGUT-ZBUKR EQ '0038'
*            AND REGUH-RZAWE EQ 'M'.
*      WA_ARQTXT+478(01) = SPACE.
*      WA_ARQTXT+480(7) = REGUH-UBKNT.
*    ENDIF.
*
*    IF WA_REGUT-ZBUKR EQ '0038'
*                    AND REGUH-RZAWE EQ 'E'.
*      WA_ARQTXT+478(01) = SPACE.
*      WA_ARQTXT+480(7) = REGUH-UBKNT.
*    ENDIF.
*************************************************
    MODIFY it_arqtxt FROM wa_arqtxt.
  ENDLOOP.
ELSE.
*> Processamento para outros Bancos.
* Prepara dados do documento de cobrança (SIGAM) que estão
* contidos nos itens da proposta de pagamento.
    READ TABLE it_arqhsbc INTO wa_arqhsbc INDEX 1.
*> Somente para arquivo do Banco do Brasil
    IF ( wa_arqhsbc+00(03) EQ '001' ). "Tipo de Banco

      SELECT belnr xref2 mandt laufd laufi xvorl zbukr
             lifnr kunnr empfg vblnr bukrs gjahr buzei zlsch
              FROM regup
              INTO TABLE it_regup
             WHERE ( laufd EQ wa_regut-laufd )
               AND ( laufi EQ wa_regut-laufi )
               AND ( xvorl EQ wa_regut-xvorl )
               AND ( zbukr EQ wa_regut-zbukr )
      ORDER BY PRIMARY KEY.

      DELETE it_regup WHERE ( vblnr IS INITIAL ).
      DELETE ADJACENT DUPLICATES FROM it_regup COMPARING belnr xref2.
      SORT it_regup BY belnr xref2.
      READ TABLE it_regup INTO wa_regup INDEX 1.

      SELECT SINGLE *
        FROM reguh
       WHERE ( laufd EQ wa_regup-laufd )
         AND ( laufi EQ wa_regup-laufi )
         AND ( xvorl EQ wa_regup-xvorl )
         AND ( zbukr EQ wa_regup-zbukr )
         AND ( lifnr EQ wa_regup-lifnr )
         AND ( kunnr EQ wa_regup-kunnr )
         AND ( empfg EQ wa_regup-empfg )
         AND ( vblnr EQ wa_regup-vblnr ).

    ENDIF.
    CLEAR: vl_conta, vl_first,vl_zlsch.
    LOOP AT it_arqhsbc INTO wa_arqhsbc.
*> Alterações previstas para arquivo do banco HSBC
      IF ( wa_arqhsbc+00(03) EQ '399' ).

        IF ( vl_first IS INITIAL ).
          wa_arqhsbc+163(03) = '020'.
          wa_arqhsbc+166(05) = '01600'.
          vl_first           = 'X'.
        ENDIF.

        IF ( wa_arqhsbc+08(01) EQ 'V' ).
          wa_aux             = wa_arqhsbc+18.
          wa_arqhsbc+18      = wa_aux+01.
        ENDIF.

        IF ( wa_arqhsbc+13(01) EQ 'A' ).
          WRITE wa_arqhsbc+29(14) TO vl_conta RIGHT-JUSTIFIED.
          TRANSLATE vl_conta USING ' 0'.
          wa_arqhsbc+29(14)  = vl_conta.
        ENDIF.

*> Alterações para arquivo do Banco do Brasil
      ELSEIF ( wa_arqhsbc+00(03) EQ '001' ). "Tipo de Banco

        IF ( vl_first IS INITIAL ).
          IF ( wa_arqhsbc+45(02) EQ '17' ).  "Carteira de Cobrança
            wa_arqhsbc+222(03) = 'CSP'.      "Identificação de Cobrança
            wa_arqhsbc+225(03) = '000'.      "Uso exclusivo de Vans
          ENDIF.
          vl_first           = 'X'.
        ENDIF.

        IF ( wa_arqhsbc+08(01) EQ 'C' ).     "Segmento de Pagamento
          wa_arqhsbc+13(03)  = '020'.
        ENDIF.

        IF ( wa_arqhsbc+13(01) EQ 'P' ).     "Segmento de cobrança
          wa_arqhsbc+15(02)  = '01'.         "Código do Movimento
          wa_arqhsbc+57(01)  = '7'.          "Código da Carteira
          wa_arqhsbc+58(01)  = '1'.          "Forma de Cadastramto Título
          wa_arqhsbc+60(01)  = '2'.          "Identific. de emis.Boleto
          wa_arqhsbc+61(01)  = '2'.          "Identific. da Distribuição
          wa_arqhsbc+100(05) = '00000'.      "Agência encarregada cobrança
          wa_arqhsbc+105(01) = ' '.          "Dígito verificador agência
          wa_arqhsbc+220(01) = '3'.          "Código para protesto

          vl_belnr = wa_arqhsbc+205(10).
          READ TABLE it_regup INTO wa_regup
                          WITH KEY belnr = vl_belnr BINARY SEARCH.
          IF ( sy-subrc EQ 0 ).
            wa_arqhsbc+62(15) = wa_regup-xref2. "Nro documento cobrança
            vl_zlsch = wa_regup-zlsch.
          ELSE.
            CLEAR wa_arqhsbc+62(15).
          ENDIF.

          wa_arqhsbc+227(02) = '09'.         "Código da moeda
          wa_arqhsbc+229(10) = '0017460783'. "Nro contrato da operação
        ELSEIF ( wa_arqhsbc+13(01) EQ 'Q' ). "ALRS 14.11.2013
          IF  vl_zlsch = 'D'.
            wa_arqhsbc+15(02)  = '01'.         "Código do Movimento
          ENDIF.
          CLEAR vl_zlsch.
        ELSEIF ( wa_arqhsbc+13(01) EQ 'A' ).

          IF ( 'TS' CS reguh-rzawe ).
            wa_arqhsbc+17(03) = '018'.       "Cod camara centralizada
          ELSEIF ( reguh-rzawe EQ 'M' ).
            wa_arqhsbc+17(03) = '700'.       "Cod camara centralizada
          ELSE.
            wa_arqhsbc+17(03) = '000'.       "Cod camara centralizada
          ENDIF.

        ENDIF.
      ENDIF.

      MODIFY it_arqhsbc FROM wa_arqhsbc.
    ENDLOOP.
ENDIF.

vl_write_lf  = 'X'.
vl_write_eol = 'X'.

CALL FUNCTION 'GUI_DOWNLOAD'
  EXPORTING
    filename                  = vl_cami
    write_lf                  = vl_write_lf
    trunc_trailing_blanks_eol = ' '
*   write_eol                 = vl_write_eol
  TABLES
    data_tab                  = <fs_arqtxt>
  EXCEPTIONS
    OTHERS                    = 01.

IF ( sy-subrc NE 0 ).
  MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
ENDIF.
