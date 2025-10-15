*----------------------------------------------------------------------*
*       CLASS ZCL_SOLICITACAO_OV  DEFINITIO
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
class ZCL_SOLICITACAO_OV definition
  public
  final
  create public .

*"* public components of class ZCL_SOLICITACAO_OV
*"* do not include other source files here!!!
public section.

  data:
    AT_TESTE TYPE TABLE OF SOLISTI1 .

  methods ENVIO_AUTOMATICO
    importing
      !P_NRO_SOL_OV type ZSDT0051-NRO_SOL_OV
      !I_ZSDT0045 type STANDARD TABLE
      !I_TCODE type SY-TCODE
      !DIRECAO type CHAR1 optional .
  methods MONTA_HTML
    importing
      !TABLE type STANDARD TABLE
      !DIRECAO type CHAR1 optional
    exporting
      !ET_OBJ_CONT type ZSTRING .
  methods ATUALIZA_ENVIO
    importing
      !TABLE type STANDARD TABLE .
  methods MONTA_ENVIO
    importing
      !T_0045 type STANDARD TABLE
      !T_HTML type STANDARD TABLE
      !T_EMAIL type STANDARD TABLE
      !I_ASSUNTO type STRING
      !DIRECAO type CHAR1 optional .
  methods ENVIO_FRETE
    importing
      !T_0045 type STANDARD TABLE
      !EMAIL type STANDARD TABLE .
  methods CHECK_EMAIL
    importing
      !I_TABLE type STANDARD TABLE
    exporting
      !E_TABLE type ZSTRING .
  methods VERIFICA_ERROS
    importing
      !I_INSTRUCAO type STANDARD TABLE
      !I_MSG type STANDARD TABLE
      !I_FCAT type STANDARD TABLE
      !I_ABA type SY-UCOMM
    exporting
      !E_MSG type ZFITRS0002 .
  methods CHECK_ESTOURO
    importing
      !FIXACAO type ANY
      !FS_TABLE type STANDARD TABLE
      !C_TABLE type STANDARD TABLE
      !P_TABLE type STANDARD TABLE
      !T_TABLE type STANDARD TABLE
      !N_TABLE type STANDARD TABLE
      !B_TABLE type STANDARD TABLE optional
    exporting
      !ERRO_59 type ANY .
  class-methods DIA_UTIL
    importing
      !P_VENCIMENTO type ZSDT0054-VALDT
      !I_BUKRS type BUKRS optional
    exporting
      !E_SUBRC type SY-SUBRC .
  class-methods DOC_SUBSEQUENTE
    importing
      !I_VBELN type VBELN optional
      !I_UCOMM type SY-UCOMM
    returning
      value(E_ERRO) type CHAR1 .
  class-methods UPPER_LOWER
    changing
      value(EXPORT) type ANY .
  class-methods GET_IMPOSTO
    importing
      !_CLIENTE type KUNNR optional
      !_FORNECEDOR type LIFNR optional
      !_MATERIAL type MATNR optional
      !_TIPO_ORDEM type AUART optional
      !_DIRECAO type CHAR1 optional
      !_VBELN type VBELN optional
      !_POSNR type POSNR optional
      !_WERKS type WERKS_EXT optional
    returning
      value(_COEFICIENTE) type KURRF .
  class-methods CHECK_LIMITE_DESCONTO
    exporting
      !VBAK type VBAK
      !VBAP type VBAP
      !KOMV type KOMV_T
    returning
      value(RETURN) type SY-SUBRC .
  class-methods GET_FIELDNAME_STRUCTURE
    importing
      !DATA type ANY
    returning
      value(TABLE) type ABAP_KEYDESCR_TAB .
  methods INSERT_ZSDT90
    importing
      !DIRECAO type CHAR1
      !SIMULADOR type ZSDED003
      !ORDEM_OLD type VBELN
      !ORDEM_NEW type VBELN
      !MATERIAL type MATNR
      !CHARG type CHARG_D
    returning
      value(RETURN) type ZSDT0090 .
  methods GET_KBETR
    importing
      !I_VBELN type VBELN
      !I_POSNR type POSNR
      !I_ADD_IMPOSTO type CHAR1 optional
    returning
      value(_KONV) type KONV .
  methods CHECK_DESC_ABS
    importing
      !_VBELN type VBELN
      !_POSNR type POSNR
      !DES type NETWR_AP optional
      !DIR type CHAR1 optional
    returning
      value(RETURN) type KBETR .
  class-methods CHECK_CBOT
    importing
      !CBOT type CHAR6
    returning
      value(VALIDO) type CHAR1 .
  methods GET_TXT_DOMINIO
    importing
      !DOMINIO type DOMNAME
      !VALUE type DOMVALUE_L
    returning
      value(TEXT) type VAL_TEXT .
  class-methods CONVERTE_ENVIO_HTML
    importing
      !INPUT type STRING
    returning
      value(RETURN) type ZSTRING .
  class-methods GET_T052_CALC
    importing
      !DATA_IN type SY-DATUM
      !ZTERM type DZTERM
    returning
      value(DATA_OUT) type SY-DATUM .
  methods CHECK_FORNECEDOR
    importing
      !LIFNR type LIFNR
    returning
      value(ISBLOCK) type CHAR1 .
  class-methods CK_DOC_EXP
    importing
      !I_VBELN type VBELN
    returning
      value(E_LIMPAR) type CHAR01 .
  class-methods GET_IMPOSTO_V2
    importing
      !I_CLIENTE type KUNNR optional
      !I_FORNECEDOR type LIFNR optional
      !I_MATERIAL type MATNR optional
      !I_TIPO_ORDEM type AUART optional
      !I_DIRECAO type CHAR1 optional
      !I_VBELN type VBELN optional
      !I_POSNR type POSNR optional
      !I_WERKS type WERKS_EXT optional
    returning
      value(I_COEFICIENTE) type RSROA_DF_SHORT .
  class-methods GET_J_1BTXIC123
    exporting
      !E_J_1BTXIC3 type J_1BTXIC3
      !E_J_1BTXIC2 type J_1BTXIC2
      !E_J_1BTXIC1 type J_1BTXIC1 .
  PROTECTED SECTION.
*"* protected components of class ZCL_SOLICITACAO_OV
*"* do not include other source files here!!!
private section.

  constants:
    AT_CBOTL(9) value 'FHKNQUVXZ' ##NO_TEXT.
  constants:
    AT_CBOTN(10) value '0123456789' ##NO_TEXT.
*"* private components of class ZCL_SOLICITACAO_OV
*"* do not include other source files here!!!
ENDCLASS.



CLASS ZCL_SOLICITACAO_OV IMPLEMENTATION.


  METHOD ATUALIZA_ENVIO.

    DATA: IT_0045 TYPE TABLE OF ZSDT0045,
          WA_0045 TYPE ZSDT0045,
          C_X(1)  VALUE 'X'.

    MOVE TABLE TO IT_0045.

    LOOP AT IT_0045 INTO WA_0045.

*    CONCATENATE '*' WA_0045-OBJEK INTO WA_0045-OBJEK.

      UPDATE ZSDT0045 SET E_MAIL = C_X
         WHERE OBJEK       EQ WA_0045-OBJEK
           AND OBJECTTABLE EQ 'ZSDT0051'
           AND INSTRUCAO   EQ WA_0045-INSTRUCAO
           AND BUKRS       EQ WA_0045-BUKRS
           AND WERKS       EQ WA_0045-WERKS.

    ENDLOOP.

    COMMIT WORK.

  ENDMETHOD.


  METHOD CHECK_CBOT.

    VALIDO = ABAP_TRUE.

    CHECK CBOT IS NOT INITIAL.
    CHECK STRLEN( CBOT ) EQ 2.

    IF AT_CBOTL CS CBOT(1)
   AND AT_CBOTN CS CBOT+1(1).
      VALIDO = ABAP_FALSE.
    ENDIF.

  ENDMETHOD.


  METHOD CHECK_DESC_ABS.

    DATA: COEFICIENTE    TYPE P DECIMALS 5,
          _DESCONTOABSLQ TYPE P DECIMALS 3.

    SELECT SINGLE *
      FROM VBAK
      INTO @DATA(_VBAK)
      WHERE VBELN EQ @_VBELN.

    SELECT SINGLE *
      FROM VBAP
      INTO @DATA(_VBAP)
      WHERE VBELN EQ @_VBELN
        AND POSNR EQ @_POSNR.

    SELECT FROM V_KONV FIELDS * WHERE KSCHL IN ( 'ICVA' , 'ICBS' , 'RB00' ) AND KNUMV EQ @_VBAK-KNUMV INTO TABLE @DATA(IT_KONV) .

    DATA(_DESCONTOABS) = DES.

    IF _VBAP-MWSBP IS NOT INITIAL.

      TRY .
          DATA(V_ICVA) = IT_KONV[ KPOSN = _VBAP-POSNR KSCHL = 'ICVA' ]-KBETR.
        CATCH CX_SY_ITAB_LINE_NOT_FOUND.
      ENDTRY.

      TRY .
          DATA(V_ICBS) = IT_KONV[ KPOSN = _VBAP-POSNR KSCHL = 'ICBS' ]-KBETR.
        CATCH CX_SY_ITAB_LINE_NOT_FOUND.
      ENDTRY.

      TRY .
          V_ICVA = V_ICVA / IT_KONV[ KPOSN = _VBAP-POSNR KSCHL = 'ICVA' ]-KAWRT.
        CATCH CX_SY_ZERODIVIDE.
      ENDTRY.

      TRY .
          V_ICBS = V_ICBS / IT_KONV[ KPOSN = _VBAP-POSNR KSCHL = 'ICBS' ]-KAWRT.
        CATCH CX_SY_ZERODIVIDE.
      ENDTRY.

      TRY .
          COEFICIENTE = 1 - ( ( V_ICBS * ( V_ICVA / 100 ) ) / 100 ).
        CATCH CX_SY_ZERODIVIDE.
      ENDTRY.

      IF DIR IS INITIAL.
        _DESCONTOABSLQ  = _DESCONTOABS * COEFICIENTE. "Remove o Imposto
      ELSE.
        _DESCONTOABSLQ  = _DESCONTOABS / COEFICIENTE. "Adiciona o Imposto
      ENDIF.

    ENDIF.

    RETURN = COND #( WHEN _DESCONTOABSLQ IS INITIAL THEN _DESCONTOABS ELSE _DESCONTOABSLQ ).
  ENDMETHOD.


  METHOD CHECK_EMAIL.

    TYPES:
      BEGIN OF TY_EMAIL,
        LINE(255) TYPE C,
      END OF TY_EMAIL.

    DATA: WA_EMAIL TYPE TY_EMAIL,
          IT_EMAIL TYPE TABLE OF TY_EMAIL,
          LEN1     TYPE I,
          OFST1    TYPE I,
          STR_C    TYPE STRING.

    BREAK ABAP.
    LOOP AT I_TABLE INTO WA_EMAIL.

      LEN1 = STRLEN( WA_EMAIL-LINE ).
      TRANSLATE WA_EMAIL-LINE  TO UPPER CASE.

      DO.

        IF OFST1 = LEN1.
          EXIT.
        ENDIF.

        IF WA_EMAIL-LINE+OFST1(1) CO CL_ABAP_CHAR_UTILITIES=>CR_LF(1).
          OFST1 = OFST1 + 1.
        ELSE.
          CONCATENATE STR_C WA_EMAIL-LINE+OFST1(1) INTO STR_C.
        ENDIF.

        OFST1 = OFST1 + 1.

      ENDDO.

      SPLIT STR_C AT ';' INTO TABLE E_TABLE.

    ENDLOOP.

    SORT E_TABLE.

    DELETE ADJACENT DUPLICATES FROM E_TABLE COMPARING ALL FIELDS.

  ENDMETHOD.


  METHOD check_estouro.

    DATA: t_new_line TYPE REF TO data,
          wl_premio  TYPE dmbtr,
          tabix_t    TYPE sy-tabix,
          tabix_p    TYPE sy-tabix,
          tabix_c    TYPE sy-tabix,
          tabix_n    TYPE sy-tabix, " 29.02.2024 - 135234 - RAMON
          tabix_b    TYPE sy-tabix. " 22.10.2024 - 154003 - RAMON

    CLEAR erro_59.

    DATA ls_bezeis TYPE c LENGTH 10.

    FIELD-SYMBOLS: <fs_line>    TYPE any,
                   <t_new_line> TYPE REF TO data,
                   <fs_campo>   TYPE any.

    CREATE DATA t_new_line LIKE LINE OF fs_table.
    ASSIGN t_new_line->* TO <fs_line>.

    LOOP AT fs_table INTO <fs_line>.

      ASSIGN COMPONENT 'POSNR' OF STRUCTURE <fs_line> TO <fs_campo>.
      IF fixacao NE <fs_campo>.
        CONTINUE.
      ENDIF.

      ASSIGN COMPONENT 'BEZEI' OF STRUCTURE <fs_line> TO <fs_campo>.
      CLEAR wl_premio.

      " 29.02.2024 - 135234 - RAMON	-->
      IF strlen( <fs_campo> ) <= 3.

        IF <fs_campo>(1) NE '0'.

          IF ls_bezeis NA <fs_campo>(1).
            ls_bezeis = ls_bezeis && <fs_campo>(1).
          ENDIF.

        ENDIF.

      ENDIF.
      " 29.02.2024 - 135234 - RAMON	--<

**********************************TAXA**********************************
      IF <fs_campo> IN t_table.

        ASSIGN COMPONENT 'QTDFIXADA' OF STRUCTURE <fs_line> TO <fs_campo>.

        REPLACE REGEX '[,]' IN <fs_campo> WITH ''.
        REPLACE REGEX '[.]' IN <fs_campo> WITH ''.
        wl_premio = <fs_campo>.

        IF wl_premio NE 0.
          tabix_t = sy-tabix.
        ENDIF.

      ENDIF.

**********************************PREMIO**********************************
      IF <fs_campo> IN p_table.

        ASSIGN COMPONENT 'QTDFIXADA' OF STRUCTURE <fs_line> TO <fs_campo>.

        REPLACE REGEX '[,]' IN <fs_campo> WITH ''.
        REPLACE REGEX '[.]' IN <fs_campo> WITH ''.
        wl_premio = <fs_campo>.

        IF wl_premio NE 0.
          tabix_p = sy-tabix.
        ENDIF.

      ENDIF.

**********************************CHICAGO**********************************
      IF <fs_campo> IN c_table.

        ASSIGN COMPONENT 'QTDFIXADA' OF STRUCTURE <fs_line> TO <fs_campo>.

        REPLACE REGEX '[,]' IN <fs_campo> WITH ''.
        REPLACE REGEX '[.]' IN <fs_campo> WITH ''.
        wl_premio = <fs_campo>.

        IF wl_premio NE 0.
          tabix_c = sy-tabix.
        ENDIF.
      ENDIF.

      " 29.02.2024 - 135234 - RAMON	-->
      IF <fs_campo> IN n_table.

        ASSIGN COMPONENT 'QTDFIXADA' OF STRUCTURE <fs_line> TO <fs_campo>.

        REPLACE REGEX '[,]' IN <fs_campo> WITH ''.
        REPLACE REGEX '[.]' IN <fs_campo> WITH ''.
        wl_premio = <fs_campo>.

        IF wl_premio NE 0.
          tabix_n = sy-tabix.
        ENDIF.
      ENDIF.
      " 29.02.2024 - 135234 - RAMON	--<

***      " 22.10.2024 - 154003 - RAMON -->
***      IF <fs_campo> IN b_table.
***
***        ASSIGN COMPONENT 'QTDFIXADA' OF STRUCTURE <fs_line> TO <fs_campo>.
***
***        REPLACE REGEX '[,]' IN <fs_campo> WITH ''.
***        REPLACE REGEX '[.]' IN <fs_campo> WITH ''.
***        wl_premio = <fs_campo>.
***
***        IF wl_premio NE 0.
***          tabix_b = sy-tabix.
***        ENDIF.
***      ENDIF.
***      " 22.10.2024 - 154003 - RAMON --<

    ENDLOOP.

* Verifica a proxima linha
    ADD 1 TO tabix_c.
    ADD 1 TO tabix_t.
    ADD 1 TO tabix_p.
    ADD 1 TO tabix_n." 29.02.2024 - 135234 - RAMON
    ADD 1 TO tabix_b." 22.10.2024 - 154003 - RAMON -->

    LOOP AT fs_table INTO <fs_line>.

      ASSIGN COMPONENT 'POSNR' OF STRUCTURE <fs_line> TO <fs_campo>.
      IF fixacao NE <fs_campo>.
        CONTINUE.
      ENDIF.

**********************************TAXA**********************************
      IF tabix_t EQ sy-tabix.
        ASSIGN COMPONENT 'BEZEI' OF STRUCTURE <fs_line> TO <fs_campo>.
        IF <fs_campo> IN t_table.
        ELSE.

          " 29.02.2024 - 135234 - RAMON	-->
          IF 'T' NA ls_bezeis.
            CONTINUE.
          ENDIF.
          " 29.02.2024 - 135234 - RAMON	--<


          IF erro_59 IS INITIAL.
            erro_59 = |E_T|.
          ELSE.
            erro_59 = |{ erro_59 }, E_T|.
          ENDIF.
        ENDIF.
      ENDIF.

**********************************PREMIO**********************************
      IF tabix_p EQ sy-tabix.
        ASSIGN COMPONENT 'BEZEI' OF STRUCTURE <fs_line> TO <fs_campo>.
        IF <fs_campo> IN p_table.
        ELSE.

          " 29.02.2024 - 135234 - RAMON	-->
          IF 'P' NA ls_bezeis.
            CONTINUE.
          ENDIF.
          " 29.02.2024 - 135234 - RAMON	--<

          IF erro_59 IS INITIAL.
            erro_59 = |E_P|.
          ELSE.
            erro_59 = |{ erro_59 }, E_P|.
          ENDIF.
        ENDIF.
      ENDIF.


**********************************CHICAGO**********************************
      IF tabix_c EQ sy-tabix.
        ASSIGN COMPONENT 'BEZEI' OF STRUCTURE <fs_line> TO <fs_campo>.
        IF <fs_campo> IN c_table.
        ELSE.

          " 29.02.2024 - 135234 - RAMON	-->
          IF 'C' NA ls_bezeis.
            CONTINUE.
          ENDIF.
          " 29.02.2024 - 135234 - RAMON	--<

          IF erro_59 IS INITIAL.
            erro_59 = |E_C|.
          ELSE.
            erro_59 = |{ erro_59 }, E_C|.
          ENDIF.
        ENDIF.
      ENDIF.

      " 29.02.2024 - 135234 - RAMON	-->

      IF tabix_n EQ sy-tabix.
        ASSIGN COMPONENT 'BEZEI' OF STRUCTURE <fs_line> TO <fs_campo>.
        IF <fs_campo> IN n_table.
        ELSE.

          " 29.02.2024 - 135234 - RAMON	-->
          IF 'N' NA ls_bezeis.
            CONTINUE.
          ENDIF.
          " 29.02.2024 - 135234 - RAMON	--<

          IF erro_59 IS INITIAL.
            erro_59 = |E_N|.
          ELSE.
            erro_59 = |{ erro_59 }, E_N|.
          ENDIF.
        ENDIF.
      ENDIF.

      " 29.02.2024 - 135234 - RAMON	--<

      " 22.10.2024 - 154003 - RAMON -->

      IF tabix_b EQ sy-tabix.

        ASSIGN COMPONENT 'BEZEI' OF STRUCTURE <fs_line> TO <fs_campo>.

        IF <fs_campo> IN b_table.

        ELSE.

          " 22.10.2024 - 154003 - RAMON -->
          IF 'B' NA ls_bezeis.
            CONTINUE.
          ENDIF.
          " 22.10.2024 - 154003 - RAMON <--

          IF erro_59 IS INITIAL.
            erro_59 = |E_B|.
          ELSE.
            erro_59 = |{ erro_59 }, E_B|.
          ENDIF.

        ENDIF.

      ENDIF.

      " 22.10.2024 - 154003 - RAMON --<

      " 16.02.2024 - RAMON -->
      " QUANDO FOR REDISTRIBUIÇÃO NAO DA ERRO.
      IF erro_59 IS NOT INITIAL.

        ASSIGN COMPONENT 'REDIST' OF STRUCTURE <fs_line> TO <fs_campo>.

        IF <fs_campo> = abap_true.
          CLEAR erro_59.
        ENDIF.

      ENDIF.

      " 16.02.2024 - RAMON --<

    ENDLOOP.

  ENDMETHOD.


  METHOD check_fornecedor.

*    SELECT SINGLE *
*      FROM lfa1
*        INTO DATA(w_lfa1)
*          WHERE lifnr EQ lifnr.
*
*    CASE abap_true.
*      WHEN b_kna1-sperr OR "  Bloqueio Geral p/ todas empresas
*           b_kna1-aufsd OR "  Bloqueio de ordem centralizado para cliente
*           b_kna1-lifsd OR "  Bloqueio de remessa centralizado para cliente
*           b_kna1-faksd OR "  Bloqueio centralizado de faturamento para cliente
*           b_kna1-cassd.   "  Bloqueio de contatos central para cliente
*        MESSAGE s836(sd) DISPLAY LIKE 'E' WITH text-001. erro = abap_true. EXIT.
*    ENDCASE.
*
*    SELECT SINGLE *
*      FROM knb1
*        INTO b_knb1
*          WHERE kunnr EQ wl_header-kunnr
*            AND bukrs EQ wl_header-vkorg.
*
*    IF     NOT b_knb1-sperr IS INITIAL.
*      MESSAGE s836(sd) DISPLAY LIKE 'E' WITH text-001. erro = abap_true. EXIT.
*    ENDIF.
*
*    CLEAR: it_lfa1, vl_check.
*
*    IF NOT b_kna1-stkzn IS INITIAL.
*
*      SELECT *
*        FROM lfa1
*        INTO TABLE it_lfa1
*        WHERE stcd2 EQ b_kna1-stcd2
*          AND stcd3 EQ b_kna1-stcd3.
*
*    ELSE.
*
*      SELECT *
*        FROM lfa1 INTO TABLE it_lfa1
*        WHERE stcd1 EQ b_kna1-stcd1
*          AND stcd3 EQ b_kna1-stcd3.
*
*    ENDIF.
*
*    IF it_lfa1 IS NOT INITIAL.
*
*      SELECT *
*        FROM lfm1
*        INTO TABLE it_lfm1
*        FOR ALL ENTRIES IN it_lfa1
*        WHERE lifnr EQ it_lfa1-lifnr.
*
*      SELECT *
*        FROM lfb1
*        INTO TABLE it_lfb1
*        FOR ALL ENTRIES IN it_lfa1
*        WHERE lifnr EQ it_lfa1-lifnr.
*
*      SORT it_lfa1 BY sperr sperm sperq ASCENDING.
*      DELETE it_lfa1 WHERE sperr EQ abap_true
*                        OR sperm EQ abap_true
*                        OR sperq NE space.
*
*      IF it_lfa1 IS INITIAL.
*        MESSAGE s836(sd) DISPLAY LIKE 'E' WITH text-002.
*        erro = abap_true.
*        EXIT.
*      ELSE.
*
*        LOOP AT it_lfa1 INTO wa_lfa1.
*
*          READ TABLE it_lfm1 WITH KEY lifnr = wa_lfa1-lifnr
*                                      sperm = abap_true TRANSPORTING NO FIELDS.
*          IF sy-subrc IS NOT INITIAL.
*            READ TABLE it_lfb1 INTO DATA(w_lfb1) WITH KEY lifnr = wa_lfa1-lifnr
*                                        sperr = abap_true TRANSPORTING NO FIELDS.
*            IF sy-subrc IS NOT INITIAL.
*              vl_check = abap_true.
*              DATA(empresa) = w_lfb1-bukrs.
*            ENDIF.
*          ENDIF.
*
*        ENDLOOP.
*
*        IF vl_check IS INITIAL.
*          MESSAGE s836(sd) DISPLAY LIKE 'E' WITH text-002 && empresa && '!'.
*          erro = abap_true.
*          EXIT.
*        ENDIF.
*
*      ENDIF.
*
*    ENDIF.

  ENDMETHOD.


  METHOD CHECK_LIMITE_DESCONTO.

    DATA: LIMITE TYPE  NETWR,
          MAXIMO TYPE  NETWR,
          MINIMO TYPE  NETWR.

    CHECK: VBAK-AUART IS NOT INITIAL OR
           VBAK-SPART IS NOT INITIAL OR
           VBAK-VKORG IS NOT INITIAL OR
           VBAK-WAERK IS NOT INITIAL.

    SELECT SINGLE NETWR
      FROM ZSDT0195
      INTO LIMITE
      WHERE AUART  EQ VBAK-AUART
        AND SPART  EQ VBAK-SPART
        AND VKORG  EQ VBAK-VKORG
        AND WAERK  EQ VBAK-WAERK
        AND STATUS EQ ABAP_FALSE.

    CHECK SY-SUBRC IS INITIAL.

    DATA(QTD_RB00) =
    REDUCE INT4( INIT X = 0
                FOR L_KOMV IN KOMV
              WHERE ( KNUMV EQ VBAK-KNUMV
                  AND KPOSN EQ VBAP-POSNR
                  AND KSCHL EQ 'RB00' )
              NEXT X = X + 1
            ).

    CHECK QTD_RB00 EQ 1.

    DATA(DESCONTO) = KOMV[ KNUMV = VBAK-KNUMV
                           KPOSN = VBAP-POSNR
                           KSCHL = 'RB00' ]-KBETR.

    MAXIMO = LIMITE.
    MINIMO = LIMITE * -1.

    IF DESCONTO NOT BETWEEN MINIMO AND MAXIMO.
      RETURN = 4.
*      MESSAGE E836(SD) WITH 'Desconto Absoluto fora dos Limites Estabelecido na ZSDT0153!'.
*      MESSAGE E899 WITH |Desconto Absoluto fora dos Limites Estabelecido na ZSDT0153!|.
    ENDIF.

  ENDMETHOD.


  METHOD ck_doc_exp.

    TYPES: BEGIN OF ty_lips,
             vbeln TYPE lips-vbeln,
           END OF ty_lips,

           BEGIN OF ty_ov,
             nro_sol_ov TYPE zsdt0053-nro_sol_ov,
             posnr      TYPE zsdt0053-posnr,
             vbeln      TYPE vbrp-vbeln,
             refkey     TYPE j_1bnflin-refkey,
           END OF ty_ov,

           BEGIN OF ty_vbak,
             vbeln TYPE vbak-vbeln,
           END OF ty_vbak,

           BEGIN OF ty_j_1bnflin,
             docnum TYPE j_1bnflin-docnum,
           END OF ty_j_1bnflin.

    DATA: it_lips      TYPE TABLE OF ty_lips,
          it_ov        TYPE TABLE OF ty_ov,
          it_vbak      TYPE TABLE OF ty_vbak,
          it_j_1bnflin TYPE TABLE OF ty_j_1bnflin.

    SELECT a~nro_sol_ov, a~posnr, d~vbeln
      FROM zsdt0053 AS a
      INNER JOIN vbrp AS c ON a~remessa_exp EQ c~vgbel
      INNER JOIN vbak AS b ON c~vbeln EQ b~vgbel
      INNER JOIN vbrp AS d ON b~vbeln EQ d~vgbel
      INTO TABLE @it_ov
      WHERE a~vbeln EQ @i_vbeln.
    CHECK sy-subrc IS INITIAL.

    LOOP AT it_ov ASSIGNING FIELD-SYMBOL(<f_ov>).
      <f_ov>-refkey = <f_ov>-vbeln.
    ENDLOOP.

    SELECT docnum
      FROM j_1bnflin
      INTO TABLE it_j_1bnflin
      FOR ALL ENTRIES IN it_ov
      WHERE refkey EQ it_ov-refkey.
    CHECK sy-subrc IS INITIAL.

    SELECT *
     FROM j_1bnfe_active
     INTO TABLE @DATA(it_j_1bnfe_active)
     FOR ALL ENTRIES IN @it_j_1bnflin
     WHERE docnum EQ @it_j_1bnflin-docnum.
    CHECK sy-subrc IS INITIAL.

    READ TABLE it_j_1bnfe_active INTO DATA(wa_j_1bfe_active) INDEX 1.

    CHECK sy-subrc IS INITIAL.
    CHECK wa_j_1bfe_active-docsta EQ '1'.
    CHECK wa_j_1bfe_active-scssta NE '2'.
    CHECK wa_j_1bfe_active-cancel EQ ' '.

    UPDATE zsdt0053 SET vbeln = ' '
                        docnum_rt = '0000000000'
                        remessa_exp = ' '
                   WHERE vbeln = i_vbeln.

    e_limpar = abap_true.

  ENDMETHOD.


  METHOD CONVERTE_ENVIO_HTML.

    DATA: LEN1    TYPE I,
          LEN2    TYPE I,
          OFST1   TYPE I,
          OFST2   TYPE I,
          OFST3   TYPE I,
          VL_HTML TYPE STRING,
          STR_AUX TYPE STRING VALUE 'a b',
          STR_C   TYPE STRING.

    VL_HTML = INPUT.

    LEN1 = STRLEN( VL_HTML ).

    DO.

      IF OFST2 EQ LEN1.
        EXIT.
      ENDIF.

      IF STRLEN( VL_HTML+OFST2 ) <= 255.
        APPEND VALUE #( LINE = VL_HTML+OFST2 ) TO RETURN.
        CLEAR STR_C.
        EXIT.
      ENDIF.

      STR_C = |{ STR_C }{ VL_HTML+OFST2(255) }|.

      OFST3 = STRLEN( STR_C ).
      SUBTRACT 1 FROM OFST3.

      DO.
        IF STR_C+OFST3(1) EQ STR_AUX+1(1).
          APPEND VALUE #( LINE = STR_C+0(OFST3) ) TO RETURN.
          CLEAR STR_C.
          EXIT.
        ELSE.
          SUBTRACT 1 FROM OFST3.
        ENDIF.
      ENDDO.
      ADD OFST3 TO OFST2.

    ENDDO.

  ENDMETHOD.


  METHOD DIA_UTIL.

    CHECK NOT P_VENCIMENTO IS INITIAL.

    DATA: VG_LAST_DAY    TYPE SY-DATUM,
          VG_FIRST_DAY   TYPE SY-DATUM,
          IT_SAB_DOM_FER TYPE TABLE OF ISCAL_DAY.

    CONCATENATE P_VENCIMENTO(6) '01' INTO VG_FIRST_DAY.
    CALL FUNCTION 'BKK_GET_MONTH_LASTDAY'
      EXPORTING
        I_DATE = VG_FIRST_DAY
      IMPORTING
        E_DATE = VG_LAST_DAY.

    "USER STORY 158527 - MMSILVA - 17.01.2025 - Inicio
    DATA: lv_hol_cal       TYPE scal-hcalid,
          lv_fac_cal       TYPE scal-fcalid,
          lv_nome_processo TYPE ze_nomep.

    lv_nome_processo = sy-tcode.

    zcl_calendario=>get_calendario(
      EXPORTING
        I_BUKRS            = i_bukrs
*        I_TIPO_PROCESSO    = 'T'
*        I_NOME_PROCESSO    = lv_nome_processo
      IMPORTING
        E_HOLIDAY_CALENDAR = lv_hol_cal
        E_FACTORY_CALENDAR = lv_fac_cal ).

    IF lv_hol_cal IS INITIAL.
      lv_hol_cal = 'BR'.
    ENDIF.
    IF lv_fac_cal IS INITIAL.
      lv_fac_cal = 'ZF'.
    ENDIF.
    "USER STORY 158527 - MMSILVA - 17.01.2025 - Fim

    FREE: IT_SAB_DOM_FER.

    CALL FUNCTION 'HOLIDAY_GET'
      EXPORTING
        FACTORY_CALENDAR           = lv_fac_cal "USER STORY 158527 - MMSILVA - 17.01.2025
        DATE_FROM                  = P_VENCIMENTO
        DATE_TO                    = VG_LAST_DAY
      TABLES
        HOLIDAYS                   = IT_SAB_DOM_FER
      EXCEPTIONS
        FACTORY_CALENDAR_NOT_FOUND = 1
        HOLIDAY_CALENDAR_NOT_FOUND = 2
        DATE_HAS_INVALID_FORMAT    = 3
        DATE_INCONSISTENCY         = 4
        OTHERS                     = 5.

    READ TABLE IT_SAB_DOM_FER TRANSPORTING NO FIELDS WITH KEY DATE = P_VENCIMENTO.
    E_SUBRC = SY-SUBRC.


  ENDMETHOD.


  METHOD DOC_SUBSEQUENTE.

    CHECK I_UCOMM EQ 'POLO' OR I_UCOMM EQ 'LOES'. " POLO é o sy-ucomm do Delete do Item e LOES é do Topo

    TYPES:

      BEGIN OF TY_BSID1,
        BUKRS TYPE BSID-BUKRS,
        KUNNR TYPE BSID-KUNNR,
        VBEL2 TYPE BSID-VBEL2,
      END OF TY_BSID1,

      BEGIN OF TY_BSAD1,
        BUKRS TYPE BSAD-BUKRS,
        KUNNR TYPE BSAD-KUNNR,
        VBEL2 TYPE BSAD-VBEL2,
      END OF TY_BSAD1.

    DATA: LT_BSID    TYPE TABLE OF TY_BSID1,
          LT_BSAD    TYPE TABLE OF TY_BSAD1,
          WA_VBAK    TYPE VBAK,
          EXISTE     TYPE C,
          LV_VBELN   TYPE P,
          LV_VBELN_C TYPE STRING.

    SELECT SINGLE *
      FROM VBAK
        INTO WA_VBAK
      WHERE VBELN EQ I_VBELN.

    CALL FUNCTION 'MOVE_CHAR_TO_NUM'
      EXPORTING
        CHR = I_VBELN
      IMPORTING
        NUM = LV_VBELN.

    LV_VBELN_C = LV_VBELN.

    SELECT A~BUKRS A~KUNNR A~VBEL2
      INTO TABLE LT_BSID
      FROM BSID AS A
      INNER JOIN BKPF AS B ON A~BUKRS = B~BUKRS
                          AND A~BELNR = B~BELNR
                          AND A~GJAHR = B~GJAHR
    WHERE A~BUKRS EQ WA_VBAK-VKORG
      AND A~KUNNR EQ WA_VBAK-KUNNR
      AND B~XREVERSAL EQ ''
      AND ( ( A~VBEL2 EQ WA_VBAK-VBELN ) OR
            ( A~XBLNR EQ WA_VBAK-VBELN ) OR
            ( A~VBEL2 EQ LV_VBELN_C ) OR
            ( A~XBLNR EQ LV_VBELN_C )  ).

    SELECT A~BUKRS A~KUNNR A~VBEL2
      INTO TABLE LT_BSAD
      FROM BSAD AS A
      INNER JOIN BKPF AS B ON A~BUKRS = B~BUKRS
                          AND A~BELNR = B~BELNR
                          AND A~GJAHR = B~GJAHR
     WHERE A~BUKRS EQ WA_VBAK-VKORG
       AND A~KUNNR EQ WA_VBAK-KUNNR
       AND B~XREVERSAL EQ ''
       AND ( ( A~VBEL2 EQ WA_VBAK-VBELN ) OR
             ( A~XBLNR EQ WA_VBAK-VBELN ) OR
             ( A~VBEL2 EQ LV_VBELN_C ) OR
             ( A~XBLNR EQ LV_VBELN_C )  ).

    IF ( LT_BSID[] IS NOT INITIAL ) OR ( LT_BSAD[] IS NOT INITIAL ).
      E_ERRO = ABAP_TRUE.
    ENDIF.

  ENDMETHOD.


  method envio_automatico.

    data: it_zsdt0045          type table of zsdt0045,
          it_0045_aux          type table of zsdt0045,
          it_0045              type table of zsdt0045,
          it_obj_cont          type table of solisti1,
          it_zmail             type table of zmail,
          wa_zsdt0045          type zsdt0045,
          wa_0045              type zsdt0045,
          wa_obj_cont          type solisti1,
          wa_zmail             type zmail,
          i_zsdt0051           type zsdt0051,
          v_nr_sol_ov          type zsdt0051-nro_sol_ov,
          v_instrucao(20)      type c,
          v_instrucao_anterior type zsdt0045-instrucao.
    data ok type c.
    data: p_mail        type zmail-email,
          p_assunto     type string,
          p_assunto1    type string,
          wa_field(300).


    select single *
      from zsdt0051
        into i_zsdt0051
          where nro_sol_ov eq p_nro_sol_ov.

*&Inicio Bug Solto 150037 / AOENNING &
    if i_zsdt0051-param_espec is not initial.
      "Seleção de dados stvarv / Z_ZSDT0121_PARAM_ESPEC
      select single * from tvarvc into @data(wa_param_espec)
      where name = 'Z_ZSDT0121_PARAM_ESPEC'
        and low  = @i_zsdt0051-param_espec.
    endif.

    if wa_param_espec is initial.
      MESSAGE I024(SD) WITH 'Parametros especial tipo de venda'
                            i_zsdt0051-param_espec
                            'não cadastrado stvarv '
                            'Z_ZSDT0121_PARAM_ESPEC'.
      EXIT.
    ENDIF.

*&Fim Bug Solto 150037 / AOENNING &

*    case i_zsdt0051-param_espec.  "Bug Solto 150037 / AOENNING
*
*      when 'A' or 'X'.  "Bug Solto 150037 / AOENNING

      move i_zsdt0051-nro_sol_ov to v_nr_sol_ov.
      shift v_nr_sol_ov left deleting leading '0'.


      check not i_zsdt0045 is initial.

      move i_zsdt0045  to it_zsdt0045.
      move it_zsdt0045 to it_0045.

      sort it_0045 by instrucao bukrs werks safra.

      delete adjacent duplicates from it_0045 comparing instrucao bukrs werks safra.

      sort it_0045 by instrucao.
*        LOOP AT IT_0045 INTO WA_0045.
*
*          IF V_INSTRUCAO_ANTERIOR NE WA_0045-INSTRUCAO.
*            V_INSTRUCAO = V_INSTRUCAO && '/' && WA_0045-INSTRUCAO.
*          ENDIF.
*
*          V_INSTRUCAO_ANTERIOR  = WA_0045-INSTRUCAO.
*          EXIT.
*
*        ENDLOOP.
*        clear WA_0045.
*        READ TABLE IT_0045 INTO WA_0045 INDEX 1.
*        V_INSTRUCAO  = WA_0045-INSTRUCAO.
*        CONCATENATE TEXT-001  V_INSTRUCAO  INTO P_ASSUNTO SEPARATED BY SPACE.

      loop at it_0045 into wa_0045.

        clear: p_assunto, p_assunto1, v_instrucao.
        v_instrucao  = wa_0045-instrucao.
        concatenate 'Inst. ' v_instrucao  into p_assunto separated by space.

        free: it_zmail.
        select * from zmail
          into table it_zmail
          where bukrs eq wa_0045-bukrs
          and   werks eq wa_0045-werks
          and   tcode eq i_tcode
          and   param_espec eq i_zsdt0051-param_espec.

        free: it_0045_aux.

        clear ok.
        loop at it_zsdt0045 into wa_zsdt0045 where e_mail is initial. "Itens
          if wa_zsdt0045-bukrs     eq wa_0045-bukrs and
             wa_zsdt0045-werks     eq wa_0045-werks  and
             wa_zsdt0045-instrucao eq wa_0045-instrucao and
             wa_zsdt0045-safra     eq wa_0045-safra.

            append wa_zsdt0045 to it_0045_aux.

            if ok is initial.
              p_assunto1 = |{ wa_zsdt0045-objek alpha = out }*{ wa_zsdt0045-werks+2(2) }|.
              condense p_assunto1 no-gaps.
              p_assunto = |{ p_assunto }*{ p_assunto1 }|.
            endif.

            ok = abap_true.

          endif.

        endloop.

        if not it_0045_aux is initial.

          sort it_0045_aux by contrato.

          me->monta_html( exporting table       = it_0045_aux
                          importing et_obj_cont = it_obj_cont ).


          me->monta_envio( t_0045    = it_0045_aux
                           t_html    = it_obj_cont
                           t_email   = it_zmail
                           i_assunto = p_assunto ).

        endif.
      endloop.
*    endcase. "Bug Solto 150037 / AOENNING

    endmethod.


  METHOD ENVIO_FRETE.

    TYPES: BEGIN OF TY_EMAIL,
             LINE(250),
           END OF TY_EMAIL.

    TYPES: BEGIN OF TY_0045_AUX.
             INCLUDE TYPE ZSDT0045.
             TYPES:  LGORT       TYPE ZPPT0002-LGORT,
             QTDEPQ      TYPE ZSDT0045-QUANTIDADE,
             QTDEG       TYPE ZSDT0045-QUANTIDADE,
             QTDP_TOTAL  TYPE ZSDT0045-QUANTIDADE,
             QTDG_TOTAL  TYPE ZSDT0045-QUANTIDADE,
             DESC_FILIAL TYPE NAME1,
             TIPO        TYPE ZPPT0004-TIPO,
           END OF TY_0045_AUX.

    TYPES: BEGIN OF TY_0002.
             INCLUDE TYPE ZPPT0002.
             TYPES:  TIPO TYPE ZPPT0004-TIPO,
           END OF TY_0002.


    DATA: IT_0045      TYPE TABLE OF TY_0045_AUX,
          IT_0045_AUX  TYPE TABLE OF TY_0045_AUX,
          IT_0045_AUX2 TYPE TABLE OF TY_0045_AUX,
          IT_EMPRESA   TYPE TABLE OF TY_0045_AUX,
          IT_FILIAL    TYPE TABLE OF TY_0045_AUX,
          IT_OBJ_CONT  TYPE TABLE OF SOLISTI1,
          IT_ZMAIL     TYPE TABLE OF ZMAIL,
          IT_LOTES     TYPE TABLE OF ZPPT0002,
          IT_0002      TYPE TABLE OF TY_0002,
          IT_0004      TYPE TABLE OF ZPPT0004,
          WA_EMPRESA   TYPE TY_0045_AUX,
          WA_FILIAL    TYPE TY_0045_AUX,
          WA_0045_AUX  TYPE TY_0045_AUX,
          WA_0045_AUX2 TYPE TY_0045_AUX,
          WA_0045      TYPE TY_0045_AUX,
          WA_0002      TYPE ZPPT0002,
          WA_0004      TYPE ZPPT0004,
          WA_ZMAIL     TYPE ZMAIL,
          WA_EMAIL     TYPE TY_EMAIL,
          TOTALP       TYPE ZSDT0045-QUANTIDADE,
          TOTALG       TYPE ZSDT0045-QUANTIDADE,
          TOTAL        TYPE ZSDT0045-QUANTIDADE,
          _WERKS       TYPE WERKS_D.

    DATA: P_MAIL      TYPE ZMAIL-EMAIL,
          P_ASSUNTO   TYPE STRING,
          P_ASSUNTO_2 TYPE STRING,
          TIPO        TYPE ZPPT0004-TIPO.

    LOOP AT EMAIL INTO WA_EMAIL.
      MOVE WA_EMAIL-LINE TO WA_ZMAIL-EMAIL.
      APPEND WA_ZMAIL TO IT_ZMAIL.
    ENDLOOP.
    READ TABLE T_0045 INTO WA_0045 INDEX 1.
    "CONCATENATE TEXT-002 WA_0045-OBJEK INTO P_ASSUNTO_2.
    CONCATENATE TEXT-002 WA_0045-INSTRUCAO INTO P_ASSUNTO SEPARATED BY SPACE.


    MOVE T_0045 TO IT_0045.

*    LOOP AT IT_0045 ASSIGNING FIELD-SYMBOL(<F_0045>).
*
*      SELECT SINGLE *
*        FROM ZSDT0166
*        INTO @DATA(WA_0166)
*        WHERE ID EQ ( SELECT MAX( ID )
*                        FROM ZSDT0166
*                        WHERE LOTE EQ @<F_0045>-CHARG
*                         AND SAFRA EQ @<F_0045>-SAFRA
*                         AND WERKS EQ @<F_0045>-WERKS
*                    ).
*
*      IF SY-SUBRC IS INITIAL.
*        <F_0045>-DESC_FILIAL = WA_0166-ALGODOEIRA.
*      ENDIF.
*
*    ENDLOOP.

    LOOP AT IT_0045 ASSIGNING FIELD-SYMBOL(<F_0045>).

      SELECT SINGLE *
        FROM ZSDT0166
        INTO @DATA(WA_0166)
        WHERE ID EQ ( SELECT MAX( ID )
                        FROM ZSDT0166
                        WHERE LOTE EQ @<F_0045>-CHARG
                         AND SAFRA EQ @<F_0045>-SAFRA
                         AND WERKS EQ @<F_0045>-WERKS
                    ).

      IF SY-SUBRC IS INITIAL.
        <F_0045>-DESC_FILIAL = WA_0166-ALGODOEIRA.
      ENDIF.

      "Projeto Reestruturação Algodao 2024
      SELECT SINGLE *
        from zppt0040 INTO @DATA(lwa_zppt0040)
       WHERE werks eq @<F_0045>-WERKS
         AND lgort eq @<F_0045>-CHARG
         and safra eq @<F_0045>-safra.

      IF sy-subrc eq 0.
        <F_0045>-TIPO = lwa_zppt0040-tipo_fardo.
      else.
        SELECT SINGLE B~TIPO
          FROM ZPPT0002 AS A
          INNER JOIN ZPPT0004 AS B ON B~VERID EQ A~VERID
                                  AND B~WERKS EQ A~WERKS
          INTO TIPO
            WHERE B~WERKS    EQ <F_0045>-WERKS
              AND A~CD_SAFRA EQ <F_0045>-SAFRA
              AND A~LGORT    EQ <F_0045>-CHARG.

          IF SY-SUBRC IS INITIAL.
            <F_0045>-TIPO = TIPO.
          ENDIF.
      ENDIF.

    ENDLOOP.

    MOVE IT_0045 TO IT_EMPRESA.
    MOVE IT_0045 TO IT_FILIAL.

    SORT IT_EMPRESA BY BUKRS.
    SORT IT_FILIAL BY WERKS DESC_FILIAL.
    DELETE ADJACENT DUPLICATES FROM IT_EMPRESA COMPARING BUKRS.
    DELETE ADJACENT DUPLICATES FROM IT_FILIAL COMPARING WERKS DESC_FILIAL.


* LOOP AT IT_EMPRESA INTO WA_EMPRESA.
*      LOOP AT IT_FILIAL INTO WA_FILIAL WHERE BUKRS EQ WA_EMPRESA-BUKRS.
*
*        CLEAR: TOTALP, TOTALG, TOTAL.
*        LOOP AT IT_0045 INTO WA_0045.
*
*          IF WA_0045-BUKRS EQ WA_EMPRESA-BUKRS AND
*              WA_0045-WERKS EQ WA_FILIAL-WERKS AND
*            WA_0045-DESC_FILIAL EQ WA_FILIAL-DESC_FILIAL.
*
*            TOTAL = TOTAL + WA_0045-QUANTIDADE.
*            MOVE WA_0045 TO WA_0045_AUX2.
*
*          ENDIF.
*
*        ENDLOOP.
*        MOVE TOTAL TO WA_0045_AUX2-QUANTIDADE.
*        APPEND WA_0045_AUX2 TO IT_0045_AUX2.
*      ENDLOOP.
*    ENDLOOP.



    LOOP AT IT_EMPRESA INTO WA_EMPRESA.
      LOOP AT IT_FILIAL INTO WA_FILIAL WHERE BUKRS EQ WA_EMPRESA-BUKRS.

        CLEAR: TOTALP, TOTALG.
        LOOP AT IT_0045 INTO WA_0045.

          IF WA_0045-BUKRS EQ WA_EMPRESA-BUKRS AND
              WA_0045-WERKS EQ WA_FILIAL-WERKS AND
            WA_0045-DESC_FILIAL EQ WA_FILIAL-DESC_FILIAL
            AND WA_0045-TIPO = 'P'.

            TOTALP = TOTALP + WA_0045-QUANTIDADE.
            MOVE WA_0045 TO WA_0045_AUX2.

          ENDIF.

          IF WA_0045-BUKRS EQ WA_EMPRESA-BUKRS AND
                        WA_0045-WERKS EQ WA_FILIAL-WERKS AND
                      WA_0045-DESC_FILIAL EQ WA_FILIAL-DESC_FILIAL
                      AND WA_0045-TIPO = 'G'.

            TOTALG = TOTALG + WA_0045-QUANTIDADE.
            MOVE WA_0045 TO WA_0045_AUX2.


          ENDIF.

        ENDLOOP.

        MOVE TOTALP TO WA_0045_AUX2-QTDEPQ.
        MOVE TOTALG TO WA_0045_AUX2-QTDEG.
        APPEND WA_0045_AUX2 TO IT_0045_AUX2.
      ENDLOOP.
    ENDLOOP.

    IT_LOTES =  VALUE #( FOR LS IN IT_0045
                          (
                              WERKS    = LS-WERKS
                              CD_SAFRA = LS-SAFRA
                              LGORT    = LS-CHARG
                          )
                        ).

    SELECT *
      FROM ZPPT0002 AS A
      INNER JOIN ZPPT0004 AS B ON B~VERID EQ A~VERID
                              AND B~WERKS EQ A~WERKS
      INTO CORRESPONDING FIELDS OF TABLE IT_0002
      FOR ALL ENTRIES IN IT_LOTES
        WHERE A~WERKS    EQ IT_LOTES-WERKS
          AND CD_SAFRA EQ IT_LOTES-CD_SAFRA
          AND LGORT    EQ IT_LOTES-LGORT.


*    LOOP AT IT_0045_AUX2 ASSIGNING FIELD-SYMBOL(<F_AUX>).
*
*      IF LINE_EXISTS( IT_0002[ WERKS = <F_AUX>-WERKS
*                            CD_SAFRA = <F_AUX>-SAFRA
*                               LGORT = <F_AUX>-CHARG
*                               TIPO  = 'P' ] ).
*        "ADD <F_AUX>-QUANTIDADE TO <F_AUX>-QTDEPQ.
*        ADD <F_AUX>-QTDP_TOTAL TO <F_AUX>-QTDEPQ.
*      ENDIF.
*
*      IF LINE_EXISTS( IT_0002[ WERKS = <F_AUX>-WERKS
*                            CD_SAFRA = <F_AUX>-SAFRA
*                               LGORT = <F_AUX>-CHARG
*                               TIPO  = 'G' ] ).
*       " ADD <F_AUX>-QUANTIDADE TO <F_AUX>-QTDEG.
*        ADD <F_AUX>-QTDG_TOTAL TO <F_AUX>-QTDEG.
*      ENDIF.
*
*    ENDLOOP.

    ME->MONTA_HTML(  EXPORTING TABLE       = IT_0045_AUX2
                               DIRECAO     = 'F'
                     IMPORTING ET_OBJ_CONT = IT_OBJ_CONT
                  ).

    ME->MONTA_ENVIO( T_0045    = IT_0045_AUX2
                     T_HTML    = IT_OBJ_CONT
                     T_EMAIL   = IT_ZMAIL
                     I_ASSUNTO = P_ASSUNTO
                     DIRECAO   = 'F'
                     ).



  ENDMETHOD.


  METHOD GET_FIELDNAME_STRUCTURE.

    DATA: LO_STRUCTDESC TYPE REF TO CL_ABAP_STRUCTDESCR,
          TYPE_DESCR    TYPE REF TO CL_ABAP_TYPEDESCR,
          LO_TABLEDESCR TYPE REF TO CL_ABAP_TABLEDESCR,
          LT_SYMBOLS    TYPE CL_ABAP_STRUCTDESCR=>SYMBOL_TABLE.

    FIELD-SYMBOLS: <TABLE> TYPE ANY TABLE.

    TYPE_DESCR = CL_ABAP_TYPEDESCR=>DESCRIBE_BY_DATA( DATA ).

    CASE TYPE_DESCR->KIND.
      WHEN CL_ABAP_TYPEDESCR=>KIND_STRUCT.

        LO_STRUCTDESC ?= TYPE_DESCR.
        LT_SYMBOLS = LO_STRUCTDESC->GET_SYMBOLS( ).

      WHEN CL_ABAP_TYPEDESCR=>KIND_TABLE.

        LO_TABLEDESCR ?= TYPE_DESCR.
        TYPE_DESCR = LO_TABLEDESCR->GET_TABLE_LINE_TYPE( ).

        ASSIGN DATA TO <TABLE>.

        IF TYPE_DESCR->KIND EQ CL_ABAP_TYPEDESCR=>KIND_STRUCT.

          LO_STRUCTDESC ?= TYPE_DESCR.
          LT_SYMBOLS = LO_STRUCTDESC->GET_SYMBOLS( ).

        ENDIF.

    ENDCASE.

    TABLE = VALUE #( FOR SY IN LT_SYMBOLS ( CORRESPONDING #( SY ) ) ).

  ENDMETHOD.


  METHOD GET_IMPOSTO.

    DATA: _HOJE_INVDT TYPE SY-DATUM.
    DATA: _BWKEY TYPE BWKEY.
    DATA: _J_1BTXIC3 TYPE J_1BTXIC3,
          _J_1BTXIC1 TYPE J_1BTXIC1.
*"// Comentado para retornar a busca do impostos WBARBOSA 13/08/2025 INICIO
*    DATA: LC_DADOS   TYPE ZSDE0183,    "*-CS2025000025-#164218-27.01.2025-JT-inicio
*          LC_RETORNO TYPE ZSDT0370_T,  "*-CS2025000025-#164218-27.01.2025-JT-inicio
*          WC_RETORNO TYPE ZSDT0370.    "*-CS2025000025-#164218-27.01.2025-JT-inicio
*"// Comentado para retornar a busca do impostos WBARBOSA 13/08/2025 INICIO

    CASE _DIRECAO.
      WHEN 'D'. "Destino

        SELECT SINGLE REGIO
          FROM KNA1
          INTO @DATA(_REGIO_K)
          WHERE KUNNR EQ @_CLIENTE.

        SELECT SINGLE REGIO
          FROM LFA1
          INTO @DATA(_REGIO_L)
          WHERE LIFNR EQ @_FORNECEDOR.

        _BWKEY = CONV #( |{ _FORNECEDOR ALPHA = IN }| ).

        SELECT SINGLE OWNPR, MTORG            "*-CS2025000025-#164218-27.01.2025-JT-inicio
          FROM MBEW
          INTO @DATA(_MBEW)                   "@DATA(_ownpr) "*-CS2025000025-#164218-27.01.2025-JT-inicio
          WHERE MATNR EQ @_MATERIAL
            AND BWKEY EQ @_BWKEY.

* Início - Sara Oikawa 18.05.2020 - CS2020000500
        SELECT SINGLE MATKL
          FROM MARA
          INTO @DATA(_MATKL)
          WHERE MATNR EQ @_MATERIAL.
* Fim - Sara Oikawa 18.05.2020 - CS2020000500

*-CS2025000025-#164218-27.01.2025-JT-inicio
*"// Descomentado para retornar a busca do impostos WBARBOSA 13/08/2025 INICIO
        SELECT SINGLE *
          FROM ZSDT0008
          INTO @DATA(_ZSDT0008)
         WHERE AUART      EQ @_TIPO_ORDEM
           AND VKAUS      EQ 'I'
           AND MWSK1      EQ 'SD'
           AND UF_CENTRO  EQ @_REGIO_L
           AND UF_CLIENTE EQ @_REGIO_K
           AND OWNPR      EQ @_MBEW-OWNPR.
*"// Descomentado para retornar a busca do impostos WBARBOSA 13/08/2025 FIM

        SELECT SINGLE VKORG
          FROM T001W
          INTO @DATA(_VKORG)
         WHERE WERKS = @_BWKEY.

*"// Comentado para retornar a busca do impostos WBARBOSA 13/08/2025 INICIO
*        LC_DADOS-AUART-VALOR      = _TIPO_ORDEM.
*        LC_DADOS-VKAUS-VALOR      = 'I'.
*        LC_DADOS-MWSK1-VALOR      = 'SD'.
*        LC_DADOS-UF_CENTRO-VALOR  = _REGIO_L.
*        LC_DADOS-UF_CLIENTE-VALOR = _REGIO_K.
*        LC_DADOS-OWNPR-VALOR      = _MBEW-OWNPR.
**       lc_dados-bukrs_toma-valor = _vkorg.
*        LC_DADOS-BUKRS_EMIT-VALOR = _VKORG.
*        LC_DADOS-KUNNR-VALOR      = ABAP_ON.
*        LC_DADOS-WERKS-VALOR      = _BWKEY.
*        LC_DADOS-MATNR-VALOR      = _MATERIAL.

*        LC_RETORNO = ZCL_IMPOSTOS=>GET_TAX_IMPOSTO( I_DADOS = LC_DADOS ).
*        READ TABLE LC_RETORNO INTO WC_RETORNO INDEX 1.
*"// Comentado para retornar a busca do impostos WBARBOSA 13/08/2025 FIM

*-CS2025000025-#164218-27.01.2025-JT-fim

*****  No dia de HOJE '27-09-18' esse processo funciona.
***** Inicio
        ZCL_UTIL_SD=>CONV_DATA_US_BR( EXPORTING I_DATA = SY-DATUM
                                      RECEIVING E_DATA = _HOJE_INVDT ).

        CALL FUNCTION 'CONVERSION_EXIT_INVDT_INPUT'
          EXPORTING
            INPUT  = _HOJE_INVDT
          IMPORTING
            OUTPUT = _HOJE_INVDT.
***** Fim
*"// Comentado para retornar a busca do impostos WBARBOSA 13/08/2025 INICIO
*        IF WC_RETORNO IS NOT INITIAL.   "_zsdt0008 IS NOT INITIAL. *-CS2025000025-#164218-27.01.2025-JT
        IF _ZSDT0008 IS NOT INITIAL. "// Descomentado para retornar a busca do impostos WBARBOSA 13/08/2025 FIM
*"// Comentado para retornar a busca do impostos WBARBOSA 13/08/2025 FIM

          SELECT COUNT(*)
            FROM KNVI
            UP TO 1 ROWS
            WHERE KUNNR EQ _CLIENTE
            AND TATYP EQ 'IBRX'
            AND TAXKD EQ '2'.

          IF SY-SUBRC IS INITIAL.

            _COEFICIENTE = 0.

          ELSE.

            SELECT COUNT(*)
              FROM J_1BTXSDC
*"// Descomentado para retornar a busca do impostos WBARBOSA 13/08/2025 INICIO
*            WHERE TAXCODE EQ WC_RETORNO-J_1BTXSDC  "_zsdt0008-j_1btxsdc *-CS2025000025-#164218-27.01.2025-JT
*"// Descomentado para retornar a busca do impostos WBARBOSA 13/08/2025 FIM
            WHERE TAXCODE EQ _ZSDT0008-J_1BTXSDC
            AND CUSTUSAGE EQ '1'
            AND ICMS EQ ABAP_TRUE.

            IF SY-SUBRC IS INITIAL.

* Início - Sara Oikawa 18.05.2020 - CS2020000500
* Ajuste para atender as regras de Vendas Futura e Venda Triangular
* Incluir a busca na tabela J_1BTXIC3-GROUP = 80 antes dos demais grupos;
* Se achar nele, não deve buscar nos demais.
              SELECT SINGLE *
                 FROM J_1BTXIC3
                 INTO _J_1BTXIC3
                WHERE LAND1     EQ 'BR'
                  AND SHIPFROM  EQ _REGIO_L
                  AND SHIPTO    EQ _REGIO_K
                  AND GRUOP     EQ '80'
                  AND VALUE     EQ _CLIENTE
                  AND VALUE2    EQ _MATERIAL
                  AND VALUE3    EQ _MATKL
                  AND VALIDFROM GE _HOJE_INVDT
                  AND VALIDTO   LE _HOJE_INVDT.

              IF SY-SUBRC IS NOT INITIAL.
* Fim - Sara Oikawa 18.05.2020 - CS2020000500

                SELECT SINGLE *
                   FROM J_1BTXIC3
                   INTO _J_1BTXIC3
                  WHERE LAND1     EQ 'BR'
                    AND SHIPFROM  EQ _REGIO_L
                    AND SHIPTO    EQ _REGIO_K
                    AND GRUOP     EQ '76'
                    AND VALUE     EQ _CLIENTE
                    AND VALUE2    EQ _MATERIAL
                    AND VALIDFROM GE _HOJE_INVDT
                    AND VALIDTO   LE _HOJE_INVDT.

                IF SY-SUBRC IS NOT INITIAL.

                  SELECT SINGLE EXTWG
                    FROM MARA
                    INTO @DATA(GRUPO_MERC_EXT)
                    WHERE MATNR EQ @_MATERIAL.

                  IF GRUPO_MERC_EXT IS NOT INITIAL.
* RIM - SKM - IR120631- 16.12.22 - Início

                    SELECT SINGLE *
                       FROM J_1BTXIC3
                       INTO _J_1BTXIC3
                      WHERE LAND1    EQ 'BR'
                        AND SHIPFROM EQ _REGIO_L
                        AND SHIPTO   EQ _REGIO_K
                        AND GRUOP    EQ '78'
                        AND VALUE    EQ GRUPO_MERC_EXT
                        AND VALUE2   EQ _WERKS
                        AND VALIDFROM GE _HOJE_INVDT
                        AND VALIDTO   LE _HOJE_INVDT.
                    IF SY-SUBRC IS NOT INITIAL.
* RIM - SKM - IR120631 - 16.12.2022 - Fim

                      SELECT SINGLE *
                         FROM J_1BTXIC3
                         INTO _J_1BTXIC3
                        WHERE LAND1    EQ 'BR'
                          AND SHIPFROM EQ _REGIO_L
                          AND SHIPTO   EQ _REGIO_K
                          AND GRUOP    EQ '79'
                          AND VALUE    EQ GRUPO_MERC_EXT
                          AND VALIDFROM GE _HOJE_INVDT
                          AND VALIDTO   LE _HOJE_INVDT.
*                  AND VALIDFROM GE @SY-DATUM
*                  AND VALIDTO   LE @SY-DATUM.

                      IF SY-SUBRC IS NOT INITIAL.

                        SELECT SINGLE *
                           FROM J_1BTXIC3
                           INTO _J_1BTXIC3
                          WHERE LAND1     EQ 'BR'
                            AND SHIPFROM  EQ _REGIO_L
                            AND SHIPTO    EQ _REGIO_K
                            AND GRUOP     EQ '77'
                            AND VALUE     EQ _MATERIAL
                            AND VALIDFROM GE _HOJE_INVDT
                            AND VALIDTO   LE _HOJE_INVDT.
*                    AND VALIDFROM GE SY-DATUM
*                    AND VALIDTO   LE SY-DATUM.

                      ENDIF.
                    ENDIF.

                  ELSE.

                    SELECT SINGLE *
                        FROM J_1BTXIC3
                        INTO _J_1BTXIC3
                       WHERE LAND1    EQ 'BR'
                         AND SHIPFROM EQ _REGIO_L
                         AND SHIPTO   EQ _REGIO_K
                         AND GRUOP    EQ '77'
                         AND VALUE    EQ _MATERIAL
                         AND VALIDFROM GE _HOJE_INVDT
                         AND VALIDTO   LE _HOJE_INVDT.
*                   AND VALIDFROM GE SY-DATUM
*                   AND VALIDTO   LE SY-DATUM.

                  ENDIF.
                ENDIF.

              ENDIF.      " Sara Oikawa 18.05.2020 - CS2020000500

              IF _J_1BTXIC3 IS INITIAL.

                SELECT SINGLE *
                  FROM J_1BTXIC1
                  INTO _J_1BTXIC1
                 WHERE LAND1    EQ 'BR'
                   AND SHIPFROM EQ _REGIO_L
                   AND SHIPTO   EQ _REGIO_K.

                IF SY-SUBRC IS INITIAL.

                  IF _J_1BTXIC3-BASE IS INITIAL.
                    _J_1BTXIC3-BASE = 100.
                  ELSE.
                    _J_1BTXIC3-BASE = _J_1BTXIC1-SPECF_BASE.
                  ENDIF.

                  _J_1BTXIC3-RATE = _J_1BTXIC1-RATE.
                ENDIF.

              ENDIF.

              TRY .
                  _COEFICIENTE = 1 - ( ( _J_1BTXIC3-BASE * ( _J_1BTXIC3-RATE / 100 ) ) / 100 ).
                CATCH CX_SY_ZERODIVIDE.
              ENDTRY.

            ENDIF.
          ENDIF.
        ENDIF.

      WHEN 'O'.   "Origem

        SELECT SINGLE *
          FROM VBAK
          INTO @DATA(_VBAK)
          WHERE VBELN EQ @_VBELN.

        SELECT SINGLE *
          FROM VBAP
          INTO @DATA(_VBAP)
          WHERE VBELN EQ @_VBELN
            AND POSNR EQ @_POSNR.

        SELECT FROM V_KONV FIELDS * WHERE KSCHL IN ( 'ICVA' , 'ICBS' , 'RB00' ) AND KNUMV EQ @_VBAK-KNUMV INTO TABLE @DATA(IT_KONV) .

        IF _VBAP-MWSBP IS NOT INITIAL.

          TRY .
              DATA(V_ICVA) = IT_KONV[ KPOSN = _VBAP-POSNR KSCHL = 'ICVA' ]-KBETR.
            CATCH CX_SY_ITAB_LINE_NOT_FOUND.
          ENDTRY.

          TRY .
              DATA(V_ICBS) = IT_KONV[ KPOSN = _VBAP-POSNR KSCHL = 'ICBS' ]-KBETR.
            CATCH CX_SY_ITAB_LINE_NOT_FOUND.
          ENDTRY.

          TRY .
              V_ICVA = V_ICVA / IT_KONV[ KPOSN = _VBAP-POSNR KSCHL = 'ICVA' ]-KAWRT.
            CATCH CX_SY_ZERODIVIDE.
          ENDTRY.

          TRY .
              V_ICBS = V_ICBS / IT_KONV[ KPOSN = _VBAP-POSNR KSCHL = 'ICBS' ]-KAWRT.
            CATCH CX_SY_ZERODIVIDE.
          ENDTRY.

          TRY .
              _COEFICIENTE = 1 - ( ( V_ICBS * ( V_ICVA / 100 ) ) / 100 ).
            CATCH CX_SY_ZERODIVIDE.
          ENDTRY.

        ENDIF.

    ENDCASE.

  ENDMETHOD.


  METHOD GET_KBETR.

    SELECT SINGLE KNUMV
      FROM VBAK
      INTO @DATA(_KNUMV)
    WHERE VBELN EQ @I_VBELN.

    SELECT SINGLE * INTO @data(WA_KONV)
      FROM V_KONV
      WHERE KNUMV EQ @_KNUMV  AND
            KPOSN EQ @I_POSNR AND
            KSCHL EQ 'PR00' .


    move-corresponding WA_KONV to _KONV .

    IF I_ADD_IMPOSTO EQ ABAP_FALSE.

      _KONV-KBETR = CHECK_DESC_ABS( _VBELN = I_VBELN
                                    _POSNR = I_POSNR
                                    DES    = CONV #( WA_KONV-KBETR )
                                    DIR    = ABAP_TRUE
                                   ).
    ENDIF.
  ENDMETHOD.


  METHOD GET_T052_CALC.

    DATA: VAR_DATA          TYPE DATUM,
          VAR_DATA_COMPLETA TYPE DATUM,
          VAR_MES_AUX       TYPE C LENGTH 2,
          VAR_ANO           TYPE C LENGTH 4,
          VAR_MES           TYPE I.

    SELECT SINGLE *
     FROM T052
      INTO @DATA(GW_T052)
    WHERE ZTERM EQ @ZTERM.

    DATA_OUT = DATA_IN.

    CHECK GW_T052-ZDART EQ 'B'.

    IF NOT GW_T052-ZTAG1 IS INITIAL.
      DATA_OUT = DATA_IN + GW_T052-ZTAG1.
    ELSE.

      VAR_MES = ( ( VAR_MES + DATA_IN+4(2) ) + GW_T052-ZMONA ).
      IF ( VAR_MES > 12 ).
        VAR_MES_AUX =  GW_T052-ZMONA.
        VAR_ANO = DATA_IN(4) + 1.
      ELSE.
        VAR_MES_AUX = VAR_MES.
        VAR_ANO     = DATA_IN(4).
      ENDIF.

      VAR_DATA_COMPLETA = |{ VAR_ANO }{ VAR_MES_AUX }{ VAR_DATA+6(2) }|.
      ADD GW_T052-ZFAEL TO VAR_DATA_COMPLETA.
      DATA_OUT = VAR_DATA_COMPLETA.

    ENDIF.

  ENDMETHOD.


  METHOD GET_TXT_DOMINIO.
    SELECT SINGLE DDTEXT
      FROM DD07T
      INTO TEXT
      WHERE DOMNAME EQ DOMINIO
        AND DOMVALUE_L EQ VALUE.
  ENDMETHOD.


  METHOD insert_zsdt90.

    DATA: seq TYPE zsdt0090-sequencia.
    DATA: total TYPE vbap-kwmeng.
    DATA: matnr TYPE matnr.

    " 11.09.2023 - CONVERSAO MATNR S4 -->
    DATA lv_matnr TYPE matnr18.
    lv_matnr = |{ material ALPHA = IN }|.
    " 11.09.2023 - CONVERSAO MATNR S4 --<

    SELECT COUNT(*)
      FROM zsdt0090 INTO seq
      WHERE doc_simulacao EQ simulador.

    SELECT * FROM vbak
      INTO TABLE @DATA(it_vbak)
     WHERE vbeln IN ( @ordem_old, @ordem_new ).

    IF sy-subrc = 0.

      SELECT  * FROM vbap
        INTO TABLE @DATA(it_vbap)
       FOR ALL ENTRIES IN @it_vbak
       WHERE vbeln EQ @it_vbak-vbeln
        AND  matnr EQ @lv_matnr."@material. " 11.09.2023 - CONVERSAO MATNR S4

      IF lines( it_vbap[] ) > 2.
        "        DELETE IT_VBAP WHERE VBELN EQ ORDEM_OLD AND CHARG NE CHARG.
        DELETE it_vbap WHERE vbeln EQ ordem_new AND charg NE charg.
      ENDIF.

      SELECT  * FROM vbkd
        INTO TABLE  @DATA(it_vbkd)
        FOR ALL ENTRIES IN @it_vbak
       WHERE vbeln EQ @it_vbak-vbeln.

      SELECT FROM v_konv FIELDS * FOR ALL ENTRIES IN @it_vbak WHERE knumv EQ @it_vbak-knumv AND kschl EQ 'PR00' INTO TABLE @DATA(it_konv) .

    ENDIF.

    READ TABLE it_vbak  INTO DATA(wa_vbak) WITH KEY vbeln = ordem_old.

    READ TABLE it_vbkd INTO DATA(wa_vbkd) WITH KEY vbeln = wa_vbak-vbeln.

    READ TABLE it_vbap INTO DATA(wa_vbap) WITH KEY vbeln = wa_vbak-vbeln.

    READ TABLE it_konv INTO DATA(wa_konv) WITH KEY knumv = wa_vbak-knumv
                                                   kposn = wa_vbap-posnr.

    ADD 1 TO seq.

    return-doc_simulacao = simulador.
    return-sequencia     = seq.
    return-categoria     = direcao.
    return-usnam         = sy-uname.
    return-data_atual    = sy-datum.
    return-hora_atual    = sy-uzeit.
    return-auartv        = wa_vbak-auart.
    return-vbelv         = wa_vbap-vbeln.
    return-spartv        = wa_vbap-spart.
    return-kunnrv        = wa_vbak-kunnr.
    return-matklv        = wa_vbap-matkl.
    return-inco1v        = wa_vbkd-inco1.
    return-werksv        = wa_vbap-werks.
    return-chargv        = wa_vbap-charg.
    return-posnv         = wa_vbap-posnr.
    return-matnrv        = wa_vbap-matnr.
    return-kurrf         = wa_vbkd-kurrf.

    IF wa_vbak-auart EQ 'ZFUT'.
      return-zmengv = wa_vbap-zmeng * -1.
    ELSE.
*      READ TABLE IT_OV INTO WA_OV WITH KEY VBELN = P_VBELV
*                                           MATNR = P_MATNR.
*      IF SY-SUBRC IS INITIAL.
*        WA_0090-ZMENGV      = WA_OV-ZMENG * -1.
*      ENDIF.
    ENDIF.

    return-flag_impostov =  COND #( WHEN wa_vbap-mwsbp IS NOT INITIAL THEN abap_true ELSE abap_false ).
    return-netprv = get_kbetr( i_vbeln = wa_vbap-vbeln i_posnr = wa_vbap-posnr )-kbetr.
    return-kmeinv = get_kbetr( i_vbeln = wa_vbap-vbeln i_posnr = wa_vbap-posnr )-kmein.
    return-ziemev = wa_vbap-zieme.

    CLEAR: wa_vbak, wa_vbap, wa_vbkd, wa_konv.

    READ TABLE it_vbak INTO wa_vbak WITH KEY vbeln = ordem_new.

    READ TABLE it_vbkd INTO wa_vbkd WITH KEY vbeln = wa_vbak-vbeln.

    READ TABLE it_vbap INTO wa_vbap WITH KEY vbeln = wa_vbak-vbeln.

    READ TABLE it_konv INTO wa_konv WITH KEY knumv = wa_vbak-knumv
                                             kposn = wa_vbap-posnr.

    return-auart      = wa_vbak-auart.
    return-vbeln      = wa_vbap-vbeln.
    return-posnn      = wa_vbap-posnr.
    return-spart      = wa_vbap-spart.

    CASE wa_vbak-auart.
      WHEN 'ZTRI'.
        return-zmeng  = wa_vbap-zmeng.
      WHEN 'ZFUT'.
        return-zmeng  = wa_vbap-zmeng.
        return-netprv = wa_konv-kbetr.
        return-kmeinv = wa_konv-kmein.
      WHEN OTHERS.
        return-zmeng  = wa_vbap-kwmeng.
        CASE direcao.
          WHEN 'Y'.
            return-zmeng = return-zmeng * -1.
          WHEN 'W'.
            return-zmeng = wa_vbap-zmeng.
        ENDCASE.
    ENDCASE.

    return-netpr = get_kbetr( i_vbeln = wa_vbap-vbeln i_posnr = wa_vbap-posnr )-kbetr.
    return-kmein = get_kbetr( i_vbeln = wa_vbap-vbeln i_posnr = wa_vbap-posnr )-kmein.

    return-zieme  = wa_vbap-zieme.
    return-charg  = wa_vbap-charg.
    return-matnr  = wa_vbap-matnr.
    return-matkl  = wa_vbap-matkl.
    return-inco1  = wa_vbkd-inco1.
    return-werks  = wa_vbap-werks.
    return-kunnr  = wa_vbak-kunnr.
    return-kurrf  = wa_vbkd-kurrf.

    return-flag_imposto = COND #( WHEN wa_vbap-mwsbp IS NOT INITIAL THEN abap_true ELSE abap_false ).

    INSERT INTO zsdt0090  VALUES return.

    "    MODIFY ZSDT0090 FROM RETURN_OLD.
    "    COMMIT WORK.
  ENDMETHOD.


  METHOD MONTA_ENVIO.

    DATA: LS_TYPE         TYPE SOOD-OBJTP,
          LV_DATE         TYPE CHAR10,
          WL_EMAIL        TYPE ADR6-SMTP_ADDR,
          LV_SUB          TYPE SO_OBJ_DES,
          LO_DOCUMENT     TYPE REF TO CL_DOCUMENT_BCS,
          LO_BCS          TYPE REF TO CL_BCS,
          LO_SAPUSER_BCS  TYPE REF TO CL_SAPUSER_BCS,
          LO_RECIPIENT    TYPE REF TO IF_RECIPIENT_BCS,
          LO_EX_BCS       TYPE REF TO CX_BCS,
          LV_MESSAGE      TYPE STRING,
          IT_ZMAIL        TYPE TABLE OF ZMAIL,
          WA_ZMAIL        TYPE ZMAIL,
          VUSER           TYPE SY-UNAME,
          P_OUTENVIADO    TYPE CHAR01,
          LV_SUBJECT      TYPE SO_OBJ_DES,
          I_TYPE          TYPE  SO_OBJ_TP,
          IT_EMAIL        TYPE TABLE OF ADR6,
          LC_STRING_MAILS TYPE STRING,
          WA_EMAIL        TYPE ADR6.


    DATA: LT_MAILSUBJECT     TYPE SODOCCHGI1, "descricao assunto
          LT_MAILRECIPIENTES TYPE TABLE OF SOMLREC90, "e-mails
          WA_MAILRECIPIENTES TYPE SOMLREC90.


    LOOP  AT T_EMAIL INTO WA_ZMAIL.
      WA_MAILRECIPIENTES-REC_TYPE = 'U'.
      WA_MAILRECIPIENTES-RECEIVER = WA_ZMAIL-EMAIL.
      APPEND WA_MAILRECIPIENTES TO LT_MAILRECIPIENTES.
    ENDLOOP.

    LT_MAILSUBJECT-OBJ_LANGU = SY-LANGU.
    LT_MAILSUBJECT-OBJ_DESCR = I_ASSUNTO.
    I_TYPE = 'HTM'.

    VUSER = SY-UNAME.
    SY-UNAME = 'JOBADM'.

    CALL FUNCTION 'SO_NEW_DOCUMENT_SEND_API1'
      EXPORTING
        DOCUMENT_DATA              = LT_MAILSUBJECT
        DOCUMENT_TYPE              = I_TYPE
      TABLES
        OBJECT_CONTENT             = T_HTML
        RECEIVERS                  = LT_MAILRECIPIENTES
      EXCEPTIONS
        TOO_MANY_RECEIVERS         = 1
        DOCUMENT_NOT_SENT          = 2
        DOCUMENT_TYPE_NOT_EXIST    = 3
        OPERATION_NO_AUTHORIZATION = 4
        PARAMETER_ERROR            = 5
        X_ERROR                    = 6
        ENQUEUE_ERROR              = 7
        OTHERS                     = 8.

    SY-UNAME = VUSER.

    IF SY-SUBRC EQ 0.
      COMMIT WORK.

      IF DIRECAO NE 'F'.
        ME->ATUALIZA_ENVIO( TABLE = T_0045 ).
      ENDIF.

      MESSAGE 'Send Successfully' TYPE 'S'.
    ENDIF.



*    CALL FUNCTION 'Z_SEND_MAIL'
*      IMPORTING
*        P_OUTENVIADO = P_OUTENVIADO
*      TABLES
*        LT_CONTENTS  = T_HTML
*        LT_SMTPADR   = IT_EMAIL
*      CHANGING
*        I_USER       = SY-UNAME
*        I_TYPE       = I_TYPE
*        I_SUBJECT    = LV_SUBJECT.

*  IF P_OUTENVIADO EQ ABAP_TRUE.
*    "try
**        CALL METHOD LO_BCS->SEND( ).
**        COMMIT WORK.
*
*    SY-UNAME = VUSER.
*
*    IF DIRECAO NE 'F'.
*      ME->ATUALIZA_ENVIO( TABLE = T_0045 ).
*    ENDIF.
*
*    MESSAGE 'Send Successfully' TYPE 'S'.
*  ELSE.
*
*  ENDIF.




    "CATCH CX_BCS INTO LO_EX_BCS.
    " SY-UNAME = VUSER.
    " LV_MESSAGE = LO_EX_BCS->GET_TEXT( ).
    "ENDTRY.


*      LOOP AT T_EMAIL INTO WA_ZMAIL.
*
*      MOVE: 'HTML'         TO LS_TYPE,
*            WA_ZMAIL-EMAIL TO WL_EMAIL.
*
*      CLEAR: LO_DOCUMENT, VUSER.
*
*      LV_SUB = I_ASSUNTO.
*
*      LO_DOCUMENT = CL_DOCUMENT_BCS=>CREATE_DOCUMENT(
*      I_TYPE = 'HTM'
*      I_SUBJECT = LV_SUB
*      I_TEXT = T_HTML ).
*
*      LO_BCS = CL_BCS=>CREATE_PERSISTENT( ).
*      LO_BCS->SET_DOCUMENT( LO_DOCUMENT ).
*
*      LO_RECIPIENT = CL_CAM_ADDRESS_BCS=>CREATE_INTERNET_ADDRESS( P_EMAIL ).
*
*      LO_BCS->SET_MESSAGE_SUBJECT( IP_SUBJECT = I_ASSUNTO ).
*
*      TRY.
*          CALL METHOD LO_BCS->ADD_RECIPIENT
*            EXPORTING
*              I_RECIPIENT = LO_RECIPIENT
*              I_EXPRESS   = 'X'.
*        CATCH CX_SEND_REQ_BCS.
*      ENDTRY.
*      *---------- send document ---------------------------------------
*    VUSER = SY-UNAME.
*    SY-UNAME = 'JOBADM'.
*
*    LO_SAPUSER_BCS = CL_SAPUSER_BCS=>CREATE( SY-UNAME ).
*    LO_BCS->SET_SENDER( I_SENDER = LO_SAPUSER_BCS ).
*    LO_BCS->SET_SEND_IMMEDIATELY( 'X' ).
*
*    TRY.
*
*        CALL METHOD LO_BCS->SEND( ).
*        COMMIT WORK.
*
*        SY-UNAME = VUSER.
*
*        IF DIRECAO NE 'F'.
*          ME->ATUALIZA_ENVIO( TABLE = T_0045 ).
*        ENDIF.
*
*        MESSAGE 'Send Successfully' TYPE 'S'.
*
*      CATCH CX_BCS INTO LO_EX_BCS.
*        SY-UNAME = VUSER.
*        LV_MESSAGE = LO_EX_BCS->GET_TEXT( ).
*    ENDTRY.
*    ENDLOOP.


  ENDMETHOD.


  METHOD monta_html.

    TYPES: BEGIN OF ty_mara,
             matnr TYPE matnr,
             normt TYPE normt,
           END OF ty_mara.

    TYPES: BEGIN OF ty_0045.
             INCLUDE TYPE zsdt0045.
             TYPES:  lgort  TYPE zppt0002-lgort,
             qtdepq TYPE zsdt0045-quantidade,
             qtdeg  TYPE zsdt0045-quantidade,
           END OF ty_0045.


    DATA: wa_obj_cont      TYPE solisti1,
          wa_0045          TYPE ty_0045,
          wa_0002          TYPE zppt0002,
          wa_0004          TYPE zppt0004,
          it_0045          TYPE TABLE OF ty_0045,
          it_0002          TYPE TABLE OF zppt0002,
          it_0004          TYPE TABLE OF zppt0004,
          it_mara          TYPE TABLE OF ty_mara,
          wa_mara          TYPE ty_mara,
          wa_field(3000), "(400),
          wa_obs(300),
          soma_qtd         TYPE zsdt0045-quantidade,
          soma_qtdp        TYPE zsdt0045-quantidade,
          soma_qtdg        TYPE zsdt0045-quantidade,
          wa_lfa1          TYPE lfa1,
          wa_correto       TYPE lfa1,
          wa_terminal      TYPE lfa1,
          wa_kna1          TYPE kna1,
          werks_aux        TYPE lfa1-lifnr,
          werks            TYPE lfa1-lifnr,
          DATA(10),
          wa_0051          TYPE zsdt0051,
          c_contrato       TYPE string,
          soma_quantidades TYPE ty_0045-quantidade,
          html             TYPE string,
          table_aux        TYPE TABLE OF ty_0045.

    it_0045 = table.
    table_aux = table.

    SELECT matnr normt
      FROM mara
      INTO TABLE it_mara
      FOR ALL ENTRIES IN it_0045
      WHERE matnr EQ it_0045-matnr.


    READ TABLE table INTO wa_0045 INDEX 1.

*    IF WA_0045-PONTO_C IS INITIAL.
    MOVE wa_0045-werks   TO werks_aux.
*    ELSE.
*      MOVE WA_0045-PONTO_C TO WERKS_AUX.
*    ENDIF.

    werks = werks_aux.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = werks_aux
      IMPORTING
        output = werks_aux.

    SELECT SINGLE *
      FROM lfa1
      INTO wa_lfa1
      WHERE lifnr EQ werks_aux.

    IF wa_lfa1-name4 IS INITIAL.
      wa_lfa1-name4 = wa_lfa1-name1.
    ENDIF.

    SELECT SINGLE *
      FROM lfa1 INTO wa_terminal
        WHERE lifnr EQ wa_0045-terminal_estuf.

    SELECT SINGLE *
      FROM zsdt0051
        INTO wa_0051
          WHERE nro_sol_ov      EQ wa_0045-objek.

    SELECT SINGLE *
      FROM kna1 INTO wa_kna1
        WHERE kunnr EQ wa_0051-kunnr.

    IF NOT wa_0051-correto IS INITIAL.
      SELECT SINGLE *
        FROM lfa1 INTO wa_correto
          WHERE lifnr EQ wa_0051-correto.
    ENDIF.


*    LOOP AT IT_0045 ASSIGNING FIELD-SYMBOL(<F0045>).
*      <F0045>-LGORT = <F0045>-CHARG.
*    ENDLOOP.
*
*    SELECT *
*      FROM ZPPT0002
*      INTO CORRESPONDING FIELDS OF TABLE IT_0002
*      FOR ALL ENTRIES IN IT_0045
*        WHERE WERKS    EQ IT_0045-WERKS
*          AND CD_SAFRA EQ IT_0045-SAFRA
*          AND LGORT    EQ IT_0045-LGORT.
*
*    IF IT_0002[] IS NOT INITIAL.
*
*      SELECT *
*        FROM ZPPT0004
*        INTO CORRESPONDING FIELDS OF TABLE IT_0004
*        FOR ALL ENTRIES IN IT_0002
*      WHERE VERID EQ IT_0002-VERID
*        AND WERKS EQ IT_0002-WERKS.
*
*
*      LOOP AT IT_0002 INTO WA_0002.
*
*        READ TABLE IT_0004 INTO WA_0004 INDEX 1.
*
*        IF WA_0004-VERID = '0001'.
*          WA_0045-QTDEPQ =  WA_0045-QTDEPQ + 1.
*        ELSEIF  WA_0004-VERID = '0002'.
*          WA_0045-QTDEPQ =  WA_0045-QTDEPQ + 1.
*        ELSEIF WA_0004-VERID = '0003'.
*          WA_0045-QTDEG =  WA_0045-QTDEG + 1.
*        ELSEIF  WA_0004-VERID = '0004'.
*          WA_0045-QTDEG =  WA_0045-QTDEG + 1.
*        ENDIF.
*      ENDLOOP.
*
*    ENDIF.


    TRANSLATE wa_0045-instrucao TO UPPER CASE.
    TRANSLATE wa_0045-contrato  TO UPPER CASE.

    FREE: et_obj_cont.

    DEFINE add_html.
      WA_OBJ_CONT-LINE = &1.
      APPEND WA_OBJ_CONT TO ET_OBJ_CONT.
    END-OF-DEFINITION.

    html = html &&
          '<!DOCTYPE html> <html> <head> <title>' && text-003 && '</title>'.
*    CONCATENATE '<title>' TEXT-003 '</title>' INTO WA_FIELD.
*    ADD_HTML: WA_FIELD.

    CASE direcao.
      WHEN ''.     " Envio do E-mail Referente ao Cadastro da Instrução.
        html = html &&
                    '<style>' &&
                    'table{ border: 1px solid black; border-collapse: collapse; width: 100%}' &&
                    'tr, td{border: 1px solid black; border-collapse: collapse;}' &&
                    'th{background-color: #C0C0C0; color: black;}' &&
                    '.border{border: none;}' &&
                    '.column-1{width : 300px; text-align: center;}' &&
                    '.column-2{width: auto;}' &&
                    '.column-3{border: none;}' &&
                    '.center{text-align: center;}' &&
                    '</style>'.

      WHEN 'F'.   " Envio do e-mail Referente a Cotação de Frete.
        html = html &&
                    '<style>' &&
                      '.topo{table-layout: auto; width: 100%; border: 1px solid black;}' &&
                      '.linha{ background-color: #D9D9D9; padding: 1px;}' &&
                      '.titulo{ text-align: center; background-color: #D9D9D9; padding: 7px; color: black;}' &&
                      '.periodo{ table-layout: auto; width: 50%; border: 1px solid black;}' &&
                      'table{ table-layout: auto; border-collapse: collapse; width: 100%;}' &&
                      '.ttable{ table-layout: auto; border-collapse: collapse; width: 100%; border: 1px solid black;}' &&
                      'table.menu{ table-layout: auto; width: 10%;}' &&
                      'table.ex1{ table-layout: auto;}' &&
                      'th, td{ text-align: left; padding: 2px;}' &&
                      '.cor:nth-child(even){background-color: #f2f2f2}' &&
                      'th{ background-color: #4CAF50; padding: 4px; color: white;}' &&
                    '</style>'.
    ENDCASE.

*    ADD_HTML:
    html = html &&
                '</head>' &&
                '<dody>'.

    CASE direcao.
      WHEN ''.      " Envio do E-mail Referente ao Cadastro da Instrução.

        CONCATENATE
                '<table> <tr><th colspan="2"> <I><U><B>' text-019 '</B></U></I> </th></tr>'
        INTO wa_field.
*        ADD_HTML: WA_FIELD.
        html = html && wa_field.

        TRANSLATE wa_lfa1-name4 TO UPPER CASE.
        CONCATENATE '<tr><td class="column-1"><b>' werks '</b></td> <td class="column-2"> <b>  &nbsp;' wa_lfa1-name4 '</b></td></tr>'
        INTO wa_field.
*        ADD_HTML: WA_FIELD.
        html = html && wa_field.

        CONCATENATE
                  '<tr><td colspan="2">&nbsp;</td></tr>'
                  '<tr><th colspan="2"> <b>' text-020 ' ' wa_0045-safra '</b> </th></tr>'
        INTO wa_field SEPARATED BY space.
*        ADD_HTML: WA_FIELD.
        html = html && wa_field.

        CONCATENATE
                  '<tr><td><b>' text-021 '</b></td> <td> &nbsp;' wa_0045-instrucao  '</td></tr>'
        INTO wa_field.
*        ADD_HTML: WA_FIELD.
        html = html && wa_field.

        CONCATENATE '<tr><td><b>' text-054 '</b></td> <td> &nbsp;' wa_0051-nro_sol_ov '</td></tr>'
        INTO wa_field.
*        ADD_HTML: WA_FIELD.
        html = html && wa_field.


        CALL FUNCTION 'CONVERSION_EXIT_PDATE_OUTPUT' EXPORTING input = wa_0045-data_instr IMPORTING output = data.
        CONCATENATE '<tr><td><b>' text-022 '</b></td> <td> &nbsp;' data '</td></tr>'
        INTO wa_field.
*        ADD_HTML: WA_FIELD.
        html = html && wa_field.

        DELETE ADJACENT DUPLICATES FROM table_aux  COMPARING contrato.

*        LOOP AT TABLE INTO WA_0045.
*          CONCATENATE C_CONTRATO WA_0045-CONTRATO INTO C_CONTRATO SEPARATED BY ', '.
*        ENDLOOP.

        LOOP AT table_aux INTO wa_0045.
          CONCATENATE c_contrato wa_0045-contrato INTO c_contrato SEPARATED BY ', '.
        ENDLOOP.

        SHIFT c_contrato LEFT DELETING LEADING ','.

        CONCATENATE
                  '<tr><td><b>' text-023 '</b></td> <td> &nbsp;' c_contrato '</td></tr>'
                  '<tr><td><b>' text-052 '</b></td> <td> &nbsp;' wa_0045-booking '</td></tr>'
                  '<tr><td colspan="2">&nbsp;</td></tr>'
        INTO wa_field.
*ADD_HTML: WA_FIELD.
        html = html && wa_field.

        CALL FUNCTION 'CONVERSION_EXIT_PDATE_OUTPUT' EXPORTING input = wa_0045-data_retirada IMPORTING output = data.
        CONCATENATE '<tr><td><b>' text-024 '</b></td> <td> &nbsp;' data '</td></tr>' INTO wa_field. html = html && wa_field. "ADD_HTML: WA_FIELD.

        CALL FUNCTION 'CONVERSION_EXIT_PDATE_OUTPUT' EXPORTING input = wa_0045-data_in_porto IMPORTING output = data.
        CONCATENATE '<tr><td><b>' text-025 '</b></td> <td> &nbsp;' data '</td></tr>' INTO wa_field. html = html && wa_field. "ADD_HTML: WA_FIELD.

        CALL FUNCTION 'CONVERSION_EXIT_PDATE_OUTPUT' EXPORTING input = wa_0045-data_porto IMPORTING output = data.
        CONCATENATE '<tr><td><b>' text-026 '</b></td> <td> &nbsp;' data '</td></tr>' INTO wa_field. html = html && wa_field. "ADD_HTML: WA_FIELD.

        CALL FUNCTION 'CONVERSION_EXIT_PDATE_OUTPUT' EXPORTING input = wa_0045-deadline_draft IMPORTING output = data.
        CONCATENATE '<tr><td><b>' text-027 '</b></td> <td> &nbsp;' data '</td></tr>' INTO wa_field. html = html && wa_field. "ADD_HTML: WA_FIELD.

        CALL FUNCTION 'CONVERSION_EXIT_PDATE_OUTPUT' EXPORTING input = wa_0045-deadline_documen IMPORTING output = data.
        CONCATENATE '<tr><td><b>' text-028 '</b></td> <td> &nbsp;' data '</td></tr>' INTO wa_field. html = html && wa_field. "ADD_HTML: WA_FIELD.

        CONCATENATE
                  '<tr><td colspan="2">&nbsp;</td></tr>'
                  '<tr><th colspan="2"> <b>' text-013 '</b> </th></tr>' "TERMINAL
        INTO wa_field.
*ADD_HTML: WA_FIELD.
        html = html && wa_field.

        SHIFT wa_terminal-lifnr LEFT DELETING LEADING '0'.
        CONCATENATE
                  '<tr><td class="center"><b>' wa_terminal-lifnr '</b></td> <td> &nbsp;' wa_terminal-name1 '; ' wa_terminal-stras '; '
                  wa_terminal-ort02  '; ' wa_terminal-ort01  '; '
                  wa_terminal-regio  '; ' wa_terminal-pstlz '</td></tr>'
                  '<tr><td colspan="2">&nbsp;</td></tr>'
        INTO wa_field SEPARATED BY space.
*ADD_HTML: WA_FIELD.
        html = html && wa_field.

        CONCATENATE
                  '<tr><th colspan="2"> <b>' text-030 '</b> </th></tr>' "CODIGO TERMINAL
        INTO wa_field.
*        ADD_HTML: WA_FIELD.
        html = html && wa_field.

        SHIFT wa_kna1-kunnr LEFT DELETING LEADING '0'.
        CONCATENATE
                  '<tr><td class="center"><b>' wa_kna1-kunnr '</b></td> <td> &nbsp;' wa_kna1-name1 '; ' wa_kna1-stras '; '
                  wa_kna1-ort02  '; ' wa_kna1-ort01  '; '
                  wa_kna1-regio  '; ' wa_kna1-pstlz ' </td></tr>'
                  '<tr><td colspan="2">&nbsp;</td></tr>'
        INTO wa_field SEPARATED BY space.
*ADD_HTML: WA_FIELD.
        html = html && wa_field.

        IF NOT wa_0051-correto IS INITIAL.
          CONCATENATE
                    '<tr><th colspan="2"> <b> ' text-033 '</b> </th></tr>'
                    '<tr><td class="center"><b>' wa_correto-lifnr '</b></td> <td> ' wa_correto-name1 ' </td> </tr>'
          INTO wa_field.
*          ADD_HTML: WA_FIELD.
          html = html && wa_field.
        ENDIF.

        CONCATENATE
                  '</table>'
                  '<br>'
                  '<table>'
                  '<tr>'
        INTO wa_field.
*        ADD_HTML: WA_FIELD.
        html = html && wa_field.

        CONCATENATE
                  '<td bgcolor="#C0C0C0" class="center"> <b>' 'PT.COLETA' '</b> </td>'
                  '<td bgcolor="#C0C0C0" class="center"> <b>' 'PRECO CONTRATADO UC$/LB' '</b> </td>'
                  '<td bgcolor="#C0C0C0" class="center"> <b>' text-036 '</b> </td>'
                  '<td bgcolor="#C0C0C0" class="center"> <b>' text-055 '</b> </td>'  "*-CS2023000189-06.04.2023-#108697-JT
                  '<td bgcolor="#C0C0C0" class="center"> <b>' text-040 '</b> </td>'
                  '<td bgcolor="#C0C0C0" class="center"> <b>' 'FARDOS'   '</b> </td>'
                  '<td bgcolor="#C0C0C0" class="center"> <b>' 'CONTRATO' '</b> </td>'
                  '</tr>'
                  '<tr>'
        INTO wa_field.
*        ADD_HTML: WA_FIELD.
        html = html && wa_field.

        soma_qtd = 0.
        LOOP AT table INTO wa_0045.

*          IF WA_0045-PONTO_C IS INITIAL.
          MOVE wa_0045-werks   TO werks_aux.
*          ELSE.
*            MOVE WA_0045-PONTO_C TO WERKS_AUX.
*          ENDIF.

          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
            EXPORTING
              input  = werks_aux
            IMPORTING
              output = werks_aux.

          SELECT SINGLE *
            FROM lfa1
            INTO wa_lfa1
           WHERE lifnr EQ werks_aux.
*
*          IF WA_LFA1-NAME4 IS INITIAL.
*            WA_LFA1-NAME4 = WA_LFA1-NAME1.
*          ENDIF.


*          IF WERKS_AUX EQ '1507'.

          SELECT SINGLE *
            FROM zsdt0166
            INTO @DATA(wa_0166)
            WHERE id EQ ( SELECT MAX( id )
                            FROM zsdt0166
                            WHERE lote EQ @wa_0045-charg
                             AND safra EQ @wa_0045-safra
                             AND werks EQ @wa_0045-werks
                        ).

          IF sy-subrc IS INITIAL.
            wa_lfa1-name4 = wa_0166-algodoeira.
          ENDIF.
*          ENDIF.

          SHIFT werks_aux LEFT DELETING LEADING '0'.

          CONCATENATE
                      '<td class="center">' werks_aux '</td>' '<td class="center">'
                  "'<td class="center">' WERKS_AUX '&nbsp-&nbsp' WA_LFA1-NAME4   '</td>' '<td class="center">'
          INTO wa_field.
*          ADD_HTML: WA_FIELD.
          html = html && wa_field.

          html = html && wa_0045-dmbtr.
*          ADD_HTML: WA_0045-DMBTR.
          READ TABLE it_mara INTO wa_mara WITH KEY matnr = wa_0045-matnr.

          soma_quantidades = wa_0045-qtdepq +  wa_0045-qtdeg.

          SHIFT wa_0045-quantidade LEFT DELETING LEADING '0'.
          SHIFT soma_quantidades LEFT DELETING LEADING '0'.

          DATA(l_acts) = COND #( WHEN wa_0045-acts = abap_true THEN 'ACTS'
                                                               ELSE '' ).

          CONCATENATE
                    '</td>'
                    '<td class="center">' wa_0045-charg(4)      '</td>'
                    '<td class="center">' l_acts             '</td>' "*-CS2023000189-06.04.2023-#108697-JT
                    '<td class="center">' wa_mara-normt(4)     '</td>'
                    '<td class="center">' wa_0045-quantidade(5) '</td>'
                    "'<td class="center">' SOMA_QUANTIDADES '</td>'
                    '<td class="center">' wa_0045-contrato   '</td>'
                    '</tr>'
          INTO wa_field.
*          ADD_HTML: WA_FIELD.
          html = html && wa_field.
          "ADD SOMA_QUANTIDADES TO SOMA_QTD.
          ADD wa_0045-quantidade TO soma_qtd.
        ENDLOOP.

        SHIFT soma_qtd LEFT DELETING LEADING '0'.

        CONCATENATE
                  '<tr class="column-5">'   ""*-CS2023000189-06.04.2023-#108697
                  '<td class="border" colspan="5" align="right"><b>' text-017 '</b></td>' ""*-CS2023000189-06.04.2023-#108697-JT
                  '<td class="column-1"><b>' soma_qtd '</b></td>'
                  '<td class="border"></td>'
                  '</tr>'
                  '</table>'
        INTO wa_field.
*        ADD_HTML: WA_FIELD.
        html = html && wa_field.

        CONCATENATE
                  '<br>'
                  '<table>'
                  '<tr><th> <b>' text-039 '</b> </th></tr>'
        INTO wa_field.
*        ADD_HTML: WA_FIELD.
        html = html && wa_field.

        LOOP AT table INTO wa_0045.
          CONCATENATE
                     '<tr><td>' wa_0045-observacao '</td> </tr>'
          INTO wa_field.
*          ADD_HTML: WA_FIELD.
          html = html && wa_field.
          EXIT.
        ENDLOOP.

        CONCATENATE
             '</table>'
             '</body>'
             '</html>'
         INTO wa_field.
*        ADD_HTML: WA_FIELD.
        html = html && wa_field.


**********************************************************************************************************************************

      WHEN 'F'.   " Envio do e-mail Referente a Cotação de Frete.

        CONCATENATE
        '<table><td class="linha"></td></table>'
        '<table class="ex1">'
        '<tr>'
          '<td width="5%"><b>' text-005 '</b></td> <td width="95%">' wa_0045-contrato  '</td>'
        '</tr>'
        '<tr>'
          '<td width="5%"><b>' text-006 '</b></td> <td width="95%">' wa_0045-instrucao  '</td>'
        '</tr>'
        '<tr>'
          '<td width="5%"><b>' text-053 '</b></td> <td width="95%">' wa_0045-objek  '</td>'
        '</tr>'
        '</table>'
        '<br>'
        INTO wa_field SEPARATED BY space.
*        ADD_HTML: WA_FIELD.
        html = html && wa_field.

        CONCATENATE
                  '<table class="ttable">'
                  '<tr class="cor">'
                  '<td class="titulo"> <b>' text-008 '</b> </td>'

                  '</tr>'
                  '</table>'
          INTO wa_field SEPARATED BY space.
*        ADD_HTML: WA_FIELD.
        html = html && wa_field.

        CONCATENATE
                  '<table class="ttable">'
                  '<tr class="cor">'
                        '<th> <b>' text-012 '</b> </th>'
                        '<th> <b>' text-009 '</b> </th>'
                        '<th> <b>' text-041 '</B> </TH>'
                        '<th> <b>' text-042 '</B> </TH>'
                        '<th> <b>' text-007 '</b> </th>'
                  '</tr>'
         INTO wa_field SEPARATED BY space.
*        ADD_HTML: WA_FIELD.
        html = html && wa_field.

        soma_qtd = 0.
        soma_qtdp = 0.
        soma_qtdg = 0.

        LOOP AT table INTO wa_0045.
          CLEAR: wa_lfa1, werks_aux.

*          IF WA_0045-PONTO_C IS INITIAL.
          MOVE wa_0045-werks   TO werks_aux.
*          ELSE.
*            MOVE WA_0045-PONTO_C TO WERKS_AUX.
*          ENDIF.

          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
            EXPORTING
              input  = werks_aux
            IMPORTING
              output = werks_aux.

          SELECT SINGLE *
            FROM lfa1
            INTO wa_lfa1
           WHERE lifnr EQ werks_aux.

*          IF WA_LFA1-NAME4 IS INITIAL.
*            WA_LFA1-NAME4 = WA_LFA1-NAME1.
*          ENDIF.

*          IF WERKS_AUX EQ '1507'.

          SELECT SINGLE *
            FROM zsdt0166
            INTO wa_0166
            WHERE id EQ ( SELECT MAX( id )
                            FROM zsdt0166
                            WHERE lote EQ wa_0045-charg
                             AND safra EQ wa_0045-safra
                             AND werks EQ wa_0045-werks
                        ).

          IF sy-subrc IS INITIAL.
            wa_lfa1-name4 = wa_0166-algodoeira.
          ENDIF.
*          ENDIF.

          TRANSLATE wa_lfa1-name4 TO UPPER CASE.
          TRANSLATE wa_lfa1-ort01 TO UPPER CASE.

          soma_quantidades = wa_0045-qtdepq +  wa_0045-qtdeg.
          CONCATENATE
                    '<tr class="cor">'
                        '<td>' wa_lfa1-name4       '</td>'
                        '<td>' wa_lfa1-ort01       '</td>'
                        '<td>' wa_0045-qtdepq      '</td>'
                        '<td>' wa_0045-qtdeg       '</td>'
                        "'<td>' WA_0045-QUANTIDADE  '</td>'
                        '<td>' soma_quantidades  '</td>'
                    '</tr>'
                          INTO wa_field SEPARATED BY space.
*          ADD_HTML: WA_FIELD.
          html = html && wa_field.

          ADD wa_0045-qtdepq     TO soma_qtdp.
          ADD wa_0045-qtdeg      TO soma_qtdg.
          ADD soma_quantidades TO soma_qtd.

        ENDLOOP.

        CONCATENATE
                  '<tr class="cor">'
                      '<th>'          '</th>'
                      '<th>' text-017 '</th>'
                      '<th>' soma_qtdp'</th>'
                      '<th>' soma_qtdg'</th>'
                      '<th>' soma_qtd '</th>'
                  '</tr>'
                        INTO wa_field SEPARATED BY space.
*        ADD_HTML: WA_FIELD.
        html = html && wa_field.

        WRITE wa_0045-data_in_porto TO wa_0045-data_in_porto DD/MM/YY.
        WRITE wa_0045-data_porto    TO wa_0045-data_porto    DD/MM/YY.
        WRITE wa_0045-data_retirada TO wa_0045-data_retirada DD/MM/YY.
        WRITE wa_0045-data_eta      TO wa_0045-data_eta DD/MM/YY.


        DATA: altura1 TYPE n LENGTH 2,
              altura2 TYPE n LENGTH 2.

        altura1 = '20'.
        altura2 = '80'.

        CONCATENATE
                  '</table>'

                  '<table>'
                  '<td class="linha"> </td> '
                  '</table>'

                  '<table class="ex1">'
                  '<tr>'
                    '<td width="' altura1 '%"> <b>' text-013 '</b> </td>' '<td width="' altura2 '%">' wa_terminal-name1'</td>'
                  '</tr>'
                  '</table>'
        INTO wa_field SEPARATED BY space.
*        ADD_HTML: WA_FIELD.
        html = html && wa_field.

        CONCATENATE
                  '<table class="ex1">'
                  '<tr>'
                     '<td width="' altura1 '%"> <b>' text-014 '</b> </td>' '<td width="' altura2 '%">' wa_terminal-mcod3'</td>'
                  '</tr>'
                  '</table>'
        INTO wa_field SEPARATED BY space.
*        ADD_HTML: WA_FIELD.
        html = html && wa_field.


        CONCATENATE
                  '<table class="ex1">'
                  '<tr>'
                     '<td width="' altura1 '%"> <b>' text-018 '</b> </td>' '<td width="' altura2 '%">' wa_0045-data_retirada '</td>'
                  '</tr>'
                  '</table>'
        INTO wa_field SEPARATED BY space.
*        ADD_HTML: WA_FIELD.
        html = html && wa_field.

        CONCATENATE
                  '<table class="ex1">'
                  '<tr>'
                     '<td width="' altura1 '%"> <b>' text-010 '</b> </td>' '<td width="' altura2 '%">' wa_0045-data_in_porto 'até' wa_0045-data_porto '</td>'
                  '</tr>'
                  '</table>'
        INTO wa_field SEPARATED BY space.
*        ADD_HTML: WA_FIELD.
        html = html && wa_field.

        IF wa_0045-mapa EQ 'S'.
          wa_field = |<table class="ex1"> <tr> <td width="{ altura1 }%"> <b> { text-043 } </b> </td> <td width="{ altura2 }%"> SIM </td </tr> </table>|.
*          ADD_HTML: WA_FIELD.
          html = html && wa_field.
        ELSE.
          wa_field = |<table class="ex1"> <tr> <td width="{ altura1 }%"> <b> { text-043 } </b> </td> <td width="{ altura2 }%"> NÃO </td </tr> </table>|.
*          ADD_HTML: WA_FIELD.
          html = html && wa_field.
        ENDIF.

        IF wa_0045-fumigacao EQ 'S'.
          wa_field = |<table class="ex1"> <tr> <td width="{ altura1 }%"> <b> { text-044 } </b> </td> <td width="{ altura2 }%"> SIM </td </tr> </table>|.
*          ADD_HTML: WA_FIELD.
          html = html && wa_field.
        ELSE.
          wa_field = |<table class="ex1"> <tr> <td width="{ altura1 }%"> <b> { text-044 } </b> </td> <td width="{ altura2 }%"> NÃO </td </tr> </table>|.
*          ADD_HTML: WA_FIELD.
          html = html && wa_field.
        ENDIF.

        wa_field = |<table class="ex1"> <tr> <td width="{ altura1 }%"> <b> { text-045 } </b> </td> <td width="{ altura2 }%"> { wa_0045-hrs_fgacao } </td </tr> </table>|.
*        ADD_HTML: WA_FIELD.
        html = html && wa_field.

        wa_field = |<table class="ex1"> <tr> <td width="{ altura1 }%"> <b> { text-046 } </b> </td> <td width="{ altura2 }%"> { wa_0045-armador } </td </tr> </table>|.
*        ADD_HTML: WA_FIELD.
        html = html && wa_field.

        wa_field = |<table class="ex1"> <tr> <td width="{ altura1 }%"> <b> { text-047 } </b> </td> <td width="{ altura2 }%"> { wa_0045-free_time } </td </tr> </table>|.
*        ADD_HTML: WA_FIELD.
        html = html && wa_field.

        wa_field = |<table class="ex1"> <tr> <td width="{ altura1 }%"> <b> { text-048 } </b> </td> <td width="{ altura2 }%"> { wa_0045-booking } </td </tr> </table>|.
*        ADD_HTML: WA_FIELD.
        html = html && wa_field.

        WRITE wa_0045-deadline_draft TO wa_0045-deadline_draft DD/MM/YY.
        WRITE wa_0045-deadline_documen TO wa_0045-deadline_documen DD/MM/YY.

        wa_field = |<table class="ex1"> <tr> <td width="{ altura1 }%"> <b> { text-049 } </b> </td> <td width="{ altura2 }%"> { wa_0045-deadline_draft } </td </tr> </table>|.
*        ADD_HTML: WA_FIELD.
        html = html && wa_field.

        wa_field = |<table class="ex1"> <tr> <td width="{ altura1 }%"> <b> { text-050 } </b> </td> <td width="{ altura2 }%"> { wa_0045-deadline_documen } </td </tr> </table>|.
*        ADD_HTML: WA_FIELD.
        html = html && wa_field.

        wa_field = |<table class="ex1"> <tr> <td width="{ altura1 }%"> <b> { text-051 } </b> </td> <td width="{ altura2 }%"> { wa_0045-data_eta } </td </tr> </table> <BR>|.
*        ADD_HTML: WA_FIELD.
        html = html && wa_field.



        TRANSLATE wa_0045-observacao TO UPPER CASE.

        wa_field = |<table class="ex1"> <tr> <td width="5%"> <b> { text-011 } </b> </td> <td width="95%"> { wa_0045-observacao } </td> </tr> </table> <table>|.
*        ADD_HTML: WA_FIELD.
        html = html && wa_field.

        wa_field = | <td class="linha"> </td> </table> |.
*        ADD_HTML: WA_FIELD.
        html = html && wa_field.

    ENDCASE.

    html = html && '</body> </html>'.

    et_obj_cont = zcl_solicitacao_ov=>converte_envio_html( html ).

  ENDMETHOD.


  METHOD UPPER_LOWER.

    TYPES: BEGIN OF TY_STRING,
             STRING(255),
           END OF TY_STRING.

    DATA: WA_STRING TYPE TY_STRING,
          IT_STRING TYPE STANDARD TABLE OF TY_STRING.

    TRANSLATE EXPORT TO UPPER CASE.
    SPLIT EXPORT AT ' ' INTO TABLE IT_STRING.
    CLEAR EXPORT.

    LOOP AT IT_STRING INTO WA_STRING.
      TRANSLATE WA_STRING-STRING+1 TO LOWER CASE.
      EXPORT = |{ EXPORT } { WA_STRING-STRING }|.
    ENDLOOP.

    EXPORT = EXPORT+1.

  ENDMETHOD.


  METHOD VERIFICA_ERROS.

    TYPES: BEGIN OF TY_DEPOSITO,
             MATNR TYPE MCHB-MATNR,
             WERKS TYPE MCHB-WERKS,
             LGORT TYPE MCHB-LGORT,
             CLABS TYPE MCHB-CLABS,
             CUMLM TYPE MCHB-CUMLM,
             CINSM TYPE MCHB-CINSM,
             CEINM TYPE MCHB-CEINM,
             CSPEM TYPE MCHB-CSPEM,
             CRETM TYPE MCHB-CRETM,
             CVMLA TYPE MCHB-CVMLA,
             CVMUM TYPE MCHB-CVMUM,
             CVMIN TYPE MCHB-CVMIN,
             CVMEI TYPE MCHB-CVMEI,
             CVMSP TYPE MCHB-CVMSP,
             CVMRE TYPE MCHB-CVMRE,
             SALDO TYPE MCHB-CLABS,
           END OF TY_DEPOSITO.

    DATA: IT_MSG_RET   TYPE TABLE OF ZFIWRS0002,
          T_NEW_LINE   TYPE REF TO DATA,
          WA_MSG_RET   TYPE ZFIWRS0002,
          WA_INSTRUCAO TYPE ZEINSTRUCAO,
          WA_FCAT      TYPE LVC_S_FCAT,
          WL_LINHA(6),
          TMCHB        TYPE TABLE OF TY_DEPOSITO,
          SALDO        TYPE TABLE OF TY_DEPOSITO.

    FIELD-SYMBOLS:
      <FS_TABLE> TYPE STANDARD TABLE,
      <WA_FCAT>  TYPE LVC_S_FCAT,
      <FS_LINE>  TYPE ANY,
      <FS_CAMPO> TYPE ANY.

    UNASSIGN <FS_LINE>.
    CREATE DATA T_NEW_LINE LIKE LINE OF I_INSTRUCAO.
    ASSIGN T_NEW_LINE->* TO <FS_LINE>.

    MOVE I_MSG TO E_MSG.

    LOOP AT I_INSTRUCAO INTO WA_INSTRUCAO.
      WL_LINHA = SY-TABIX.

      MOVE-CORRESPONDING WA_INSTRUCAO TO <FS_LINE>.

      LOOP AT I_FCAT ASSIGNING <WA_FCAT>.

        ASSIGN COMPONENT <WA_FCAT>-FIELDNAME  OF STRUCTURE <FS_LINE> TO <FS_CAMPO>.

        CASE <WA_FCAT>-FIELDNAME.
          WHEN 'PONTO_C' OR 'OBSERVACAO' OR 'ICON'.
          WHEN OTHERS.

            IF <FS_CAMPO> IS INITIAL.
              MOVE: <WA_FCAT>-FIELDNAME   TO WA_MSG_RET-FIELD,
                    I_ABA                 TO WA_MSG_RET-ABA,
                    'GRID6'               TO WA_MSG_RET-OBJ,
                    WL_LINHA              TO WA_MSG_RET-TABIX.

              CONDENSE WL_LINHA NO-GAPS.

              CONCATENATE TEXT-015 '"' <WA_FCAT>-SELTEXT '"' TEXT-016 WL_LINHA
                          INTO  WA_MSG_RET-MSG SEPARATED BY SPACE.

              APPEND WA_MSG_RET TO E_MSG.
              CLEAR: WA_MSG_RET.
            ENDIF.

        ENDCASE.

      ENDLOOP.

      IF NOT WA_INSTRUCAO-WERKS IS INITIAL AND
         NOT WA_INSTRUCAO-MATNR IS INITIAL AND
         NOT WA_INSTRUCAO-CHARG IS INITIAL AND
         NOT WA_INSTRUCAO-BTGEW  IS INITIAL AND
             WA_INSTRUCAO-ZSEQ_INST   IS INITIAL AND
             WA_INSTRUCAO-OBJEK       IS INITIAL AND
             WA_INSTRUCAO-OBJECTTABLE IS INITIAL.

        FREE TMCHB.

        SELECT * FROM MCHB
          INTO CORRESPONDING FIELDS OF TABLE TMCHB
          WHERE MATNR EQ WA_INSTRUCAO-MATNR
            AND WERKS EQ WA_INSTRUCAO-WERKS
            AND LGORT EQ WA_INSTRUCAO-CHARG(4).

        IF SY-SUBRC IS INITIAL.

          FREE SALDO.

          LOOP AT TMCHB INTO DATA(WMCHB).
            COLLECT WMCHB INTO SALDO.
            CLEAR WMCHB.
          ENDLOOP.

          DATA(WSALDO) = SALDO[ 1 ].

          ADD WSALDO-CLABS TO WSALDO-SALDO.
          ADD WSALDO-CUMLM TO WSALDO-SALDO.
          ADD WSALDO-CINSM TO WSALDO-SALDO.
          ADD WSALDO-CEINM TO WSALDO-SALDO.
          ADD WSALDO-CSPEM TO WSALDO-SALDO.
          ADD WSALDO-CRETM TO WSALDO-SALDO.
          ADD WSALDO-CVMLA TO WSALDO-SALDO.
          ADD WSALDO-CVMUM TO WSALDO-SALDO.
          ADD WSALDO-CVMIN TO WSALDO-SALDO.
          ADD WSALDO-CVMEI TO WSALDO-SALDO.
          ADD WSALDO-CVMSP TO WSALDO-SALDO.
          ADD WSALDO-CVMRE TO WSALDO-SALDO.

        ELSE.
          WSALDO-SALDO = 0.
        ENDIF.

        IF WA_INSTRUCAO-BTGEW > WSALDO-SALDO.
          WA_MSG_RET-MSG = |Total da Instrução da linha { WL_LINHA } maior que o disponível { WSALDO-SALDO }!|.
          APPEND WA_MSG_RET TO E_MSG.
          CLEAR: WA_MSG_RET.
        ENDIF.

      ENDIF.

      CLEAR WSALDO.

    ENDLOOP.
  ENDMETHOD.


  METHOD GET_IMPOSTO_V2.

    DATA: LV_HOJE_INVDT TYPE SY-DATUM.
    DATA: LV_BWKEY TYPE BWKEY.
    DATA: LV_BASE TYPE J_1BTXBASE,
          LV_RATE TYPE J_1BTXRATE.

    DATA: LT_1BTXIC3   TYPE TABLE OF J_1BTXIC3,
          LS_J_1BTXIC3 TYPE J_1BTXIC3,
          LS_J_1BTXIC2 TYPE J_1BTXIC2,
          LS_J_1BTXIC1 TYPE J_1BTXIC1.

    I_COEFICIENTE = 1.

    CASE I_DIRECAO.
      WHEN 'D'.

        SELECT SINGLE REGIO
          FROM KNA1
          INTO @DATA(LV_REGIO_K)
          WHERE KUNNR EQ @I_CLIENTE.

        SELECT SINGLE REGIO
          FROM LFA1
          INTO @DATA(LV_REGIO_L)
          WHERE LIFNR EQ @I_FORNECEDOR.

        LV_BWKEY = CONV #( |{ I_FORNECEDOR ALPHA = IN }| ).

        SELECT SINGLE OWNPR, MTORG
          FROM MBEW
          INTO @DATA(_MBEW)
          WHERE MATNR EQ @I_MATERIAL
            AND BWKEY EQ @LV_BWKEY.

        SELECT SINGLE MATKL
          FROM MARA
          INTO @DATA(LV_GRUPO_MERCADORIA)
          WHERE MATNR EQ @I_MATERIAL.

        SELECT SINGLE EXTWG
        FROM MARA
        INTO @DATA(LV_GRUPO_MERCADORIA_EXTERNO)
        WHERE MATNR EQ @I_MATERIAL.

        SELECT SINGLE *
          FROM ZSDT0008
          INTO @DATA(_ZSDT0008)
         WHERE AUART      EQ @I_TIPO_ORDEM
           AND VKAUS      EQ 'I'
           AND MWSK1      EQ 'SD'
           AND UF_CENTRO  EQ @LV_REGIO_L
           AND UF_CLIENTE EQ @LV_REGIO_K
           AND OWNPR      EQ @_MBEW-OWNPR.

        CHECK _ZSDT0008 IS NOT INITIAL.

        SELECT SINGLE VKORG
          FROM T001W
          INTO @DATA(_VKORG)
         WHERE WERKS = @LV_BWKEY.

        ZCL_UTIL_SD=>CONV_DATA_US_BR( EXPORTING I_DATA = SY-DATUM
                                      RECEIVING E_DATA = LV_HOJE_INVDT ).

        CALL FUNCTION 'CONVERSION_EXIT_INVDT_INPUT'
          EXPORTING
            INPUT  = LV_HOJE_INVDT
          IMPORTING
            OUTPUT = LV_HOJE_INVDT.

        SELECT COUNT(*)
          FROM KNVI
          UP TO 1 ROWS
          WHERE KUNNR EQ I_CLIENTE
          AND TATYP EQ 'IBRX'
          AND TAXKD EQ '2'.

        CHECK SY-SUBRC IS NOT INITIAL.

        SELECT COUNT(*)
          FROM J_1BTXSDC
        WHERE TAXCODE EQ _ZSDT0008-J_1BTXSDC
        AND CUSTUSAGE EQ '1'
        AND ICMS EQ ABAP_TRUE.

        CHECK SY-SUBRC IS INITIAL.

        SELECT SINGLE *
             FROM J_1BTXIC2
             INTO LS_J_1BTXIC2
            WHERE LAND1     EQ 'BR'
              AND SHIPFROM  EQ LV_REGIO_L
              AND SHIPTO    EQ LV_REGIO_K
              AND MATNR     EQ I_MATERIAL
              AND VALIDFROM GE LV_HOJE_INVDT
              AND VALIDTO   LE LV_HOJE_INVDT.

        IF SY-SUBRC IS INITIAL.
          LV_BASE = LS_J_1BTXIC2-BASE.
          LV_RATE = LS_J_1BTXIC2-RATE.
        ELSE.

          SELECT *
             FROM J_1BTXIC3
             INTO TABLE LT_1BTXIC3
            WHERE LAND1     EQ 'BR'
              AND SHIPFROM  EQ LV_REGIO_L
              AND SHIPTO    EQ LV_REGIO_K
              AND GRUOP     IN ( '69','76','77','78','79','80' )
              AND VALIDFROM GE LV_HOJE_INVDT
              AND VALIDTO   LE LV_HOJE_INVDT.

          IF SY-SUBRC IS INITIAL.
            READ TABLE LT_1BTXIC3 INTO LS_J_1BTXIC3
            WITH KEY GRUOP  = '69'
                     VALUE2 = LV_GRUPO_MERCADORIA
                     VALUE3 = LV_GRUPO_MERCADORIA_EXTERNO.
            IF SY-SUBRC IS NOT INITIAL.

              READ TABLE LT_1BTXIC3 INTO LS_J_1BTXIC3
              WITH KEY GRUOP  = '76'
                       VALUE  = I_CLIENTE
                       VALUE2 = I_MATERIAL.
              IF SY-SUBRC IS NOT INITIAL.

                READ TABLE LT_1BTXIC3 INTO LS_J_1BTXIC3
                WITH KEY GRUOP  = '77'
                         VALUE  = I_MATERIAL.
                IF SY-SUBRC IS NOT INITIAL.

                  READ TABLE LT_1BTXIC3 INTO LS_J_1BTXIC3
                  WITH KEY GRUOP  = '78'
                           VALUE  = LV_GRUPO_MERCADORIA_EXTERNO
                           VALUE2 = I_WERKS.
                  IF SY-SUBRC IS NOT INITIAL.

                    READ TABLE LT_1BTXIC3 INTO LS_J_1BTXIC3
                    WITH KEY GRUOP  = '79'
                             VALUE  = LV_GRUPO_MERCADORIA_EXTERNO.
                    IF SY-SUBRC IS NOT INITIAL.

                      READ TABLE LT_1BTXIC3 INTO LS_J_1BTXIC3
                      WITH KEY GRUOP  = '80'
                               VALUE  = I_CLIENTE
                               VALUE2 = I_MATERIAL
                               VALUE3 = LV_GRUPO_MERCADORIA.

                    ENDIF.
                  ENDIF.
                ENDIF.
              ENDIF.
            ENDIF.
          ENDIF.

          IF LS_J_1BTXIC3 IS INITIAL.

            SELECT SINGLE *
              FROM J_1BTXIC1
              INTO LS_J_1BTXIC1
             WHERE LAND1    EQ 'BR'
               AND SHIPFROM EQ LV_REGIO_L
               AND SHIPTO   EQ LV_REGIO_K.

            IF SY-SUBRC IS INITIAL.
              LV_BASE = COND #( WHEN LS_J_1BTXIC1-SPECF_BASE IS INITIAL THEN 100 ELSE LS_J_1BTXIC1-SPECF_BASE ).
              LV_RATE = LS_J_1BTXIC1-RATE.
            ENDIF.

          ELSE.
            LV_BASE = LS_J_1BTXIC3-BASE.
            LV_RATE = LS_J_1BTXIC3-RATE.
          ENDIF.
        ENDIF.

        CHECK LV_RATE IS NOT INITIAL.
        CHECK LV_BASE IS NOT INITIAL.

        TRY .
            I_COEFICIENTE = 1 - ( ( LV_BASE * ( LV_RATE / 100 ) ) / 100 ).
          CATCH CX_SY_ZERODIVIDE.
        ENDTRY.

      WHEN 'O'.

        SELECT SINGLE *
          FROM VBAK
          INTO @DATA(_VBAK)
          WHERE VBELN EQ @I_VBELN.

        SELECT SINGLE *
          FROM VBAP
          INTO @DATA(_VBAP)
          WHERE VBELN EQ @I_VBELN
            AND POSNR EQ @I_POSNR.

        SELECT FROM V_KONV FIELDS * WHERE KSCHL IN ( 'ICVA' , 'ICBS' , 'RB00' ) AND KNUMV EQ @_VBAK-KNUMV INTO TABLE @DATA(IT_KONV) .

        IF _VBAP-MWSBP IS NOT INITIAL.

          TRY .
              DATA(V_ICVA) = IT_KONV[ KPOSN = _VBAP-POSNR KSCHL = 'ICVA' ]-KBETR.
            CATCH CX_SY_ITAB_LINE_NOT_FOUND.
          ENDTRY.

          TRY .
              DATA(V_ICBS) = IT_KONV[ KPOSN = _VBAP-POSNR KSCHL = 'ICBS' ]-KBETR.
            CATCH CX_SY_ITAB_LINE_NOT_FOUND.
          ENDTRY.

          TRY .
              V_ICVA = V_ICVA / IT_KONV[ KPOSN = _VBAP-POSNR KSCHL = 'ICVA' ]-KAWRT.
            CATCH CX_SY_ZERODIVIDE.
          ENDTRY.

          TRY .
              V_ICBS = V_ICBS / IT_KONV[ KPOSN = _VBAP-POSNR KSCHL = 'ICBS' ]-KAWRT.
            CATCH CX_SY_ZERODIVIDE.
          ENDTRY.

          TRY .
              I_COEFICIENTE = 1 - ( ( V_ICBS * ( V_ICVA / 100 ) ) / 100 ).
            CATCH CX_SY_ZERODIVIDE.
          ENDTRY.

        ENDIF.

    ENDCASE.

  ENDMETHOD.


  METHOD GET_J_1BTXIC123.

  ENDMETHOD.
ENDCLASS.
