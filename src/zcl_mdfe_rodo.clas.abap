class ZCL_MDFE_RODO definition
  public
  inheriting from ZCL_MDFE
  create public .

*"* public components of class ZCL_MDFE_RODO
*"* do not include other source files here!!!
public section.

  methods MONTA_XML
    importing
      !I_OBJ type ref to ZCL_MDFE
    returning
      value(E_XML) type STRING .
protected section.
*"* protected components of class ZCL_MDFE_RODO
*"* do not include other source files here!!!
private section.
*"* private components of class ZCL_MDFE_RODO
*"* do not include other source files here!!!

  data AT_RNTRC type NUM08 .
  data AT_CIOT type NUM12 .
  data AT_VEIC_TRACAO type ref to ZCL_MDFE_RODO_VT .

  methods GET_DADOS_TRANSP
    importing
      !I_DOCNUM type J_1BDOCNUM
    returning
      value(E_ST_VTTK) type VTTK .
ENDCLASS.



CLASS ZCL_MDFE_RODO IMPLEMENTATION.


method GET_DADOS_TRANSP.

  DATA: WA_DOC_TRANSP TYPE VTTK.

  CLEAR: WA_DOC_TRANSP, E_ST_VTTK.

  CALL FUNCTION 'Z_SD_NFES_DA_CTE'
    EXPORTING
      MP_DOCNUM  = I_DOCNUM
    CHANGING
      MR_VTTK    = WA_DOC_TRANSP.

  MOVE-CORRESPONDING WA_DOC_TRANSP TO E_ST_VTTK.



endmethod.


METHOD MONTA_XML.


  TYPES: BEGIN OF TY_PLACAS,
           PLACA(7) TYPE C,
         END OF TY_PLACAS.

  DATA: VL_PLACA(7)      TYPE C,
        VINC_VEIC_TRACAO TYPE C,
        XVALOR           TYPE STRING,
        VL_GRAVA_SN      TYPE C,
        VL_11POS(11)     TYPE C,
        VL_14POS(14)     TYPE C,
        VL_BAHNS         TYPE LFA1-BAHNS,
        VL_NAME1         TYPE LFA1-NAME1,
        VL_STCD2         TYPE LFA1-STCD2,
        VL_LIMPO         TYPE STRING,
        VL_TIPO_CAR      TYPE C,
        VL_TIPO_ROD      TYPE C.

  TYPES: BEGIN OF Y_LFA1,
           NAME1  TYPE LFA1-NAME1,
           STCD1  TYPE LFA1-STCD1,    "CNPJ
           STCD2  TYPE LFA1-STCD2,    "CPF
           STCD3  TYPE LFA1-STCD3,    "IE
           BAHNS  TYPE LFA1-BAHNS,    "Registro Nacional dos Transp.de Cargas
           REGION TYPE ADRC-REGION,   "UF
           ADRNR  TYPE LFA1-ADRNR,
         END OF Y_LFA1.

  DATA: ST_ZLEST0002 TYPE ZLEST0002,
        ST_ADRC      TYPE ADRC,
        ST_ADR6      TYPE ADR6,
        ST_LFA1      TYPE Y_LFA1.

  DATA: IT_PLACAS     TYPE TABLE OF TY_PLACAS,
        WA_PLACAS     TYPE TY_PLACAS,
        WA_VTTK       TYPE VTTK,
        IT_TRANS      TYPE TABLE OF ZCTE_TRANS,
        WA_TRANS      TYPE ZCTE_TRANS,
        VL_TP_DOC_REF TYPE ZSDT0102-TP_DOC_REF,
        VL_MOTORISTA  TYPE LIFNR.

  DATA: IT_ZLEST0002 TYPE TABLE OF ZLEST0002.

  DEFINE CONC_XML.
    CONCATENATE E_XML &1 INTO E_XML.
  END-OF-DEFINITION.

  I_OBJ->GET_TP_DOC_REF( RECEIVING E_TP_DOC_REF = VL_TP_DOC_REF ).

  REFRESH: IT_PLACAS.

  CLEAR: VINC_VEIC_TRACAO.
  CLEAR: E_XML. "Limpar a variavel de retorno.

  CONC_XML '<rodo>'.

  SELECT SINGLE * INTO @DATA(WA_CIOT)
    FROM ZCTE_CIOT WHERE DOCNUM EQ @I_OBJ->AT_WA_DOC_MDFE-DOCNUM.


  SELECT SINGLE * INTO @DATA(WA_IDENTIFICA_DOC)
    FROM ZCTE_IDENTIFICA WHERE DOCNUM EQ @I_OBJ->AT_WA_DOC_MDFE-DOCNUM.

  IF WA_CIOT-NR_CIOT IS NOT INITIAL.

    CONC_XML '<RNTRC>'.
    CONC_XML WA_IDENTIFICA_DOC-RODO_RNTRC .
    CONC_XML '</RNTRC>'.

    CONC_XML '<infCIOT>'.

    CONC_XML '<CIOT>'.
    CONC_XML WA_CIOT-NR_CIOT.
    CONC_XML '</CIOT>'.

    IF WA_CIOT-TR_CNPJ IS NOT INITIAL.
      CONC_XML '<CNPJ>'.
      CONC_XML WA_CIOT-TR_CNPJ.
      CONC_XML '</CNPJ>'.
    ENDIF.

    IF WA_CIOT-TR_CPF IS NOT INITIAL.
      CONC_XML '<CPF>'.
      CONC_XML WA_CIOT-TR_CPF.
      CONC_XML '</CPF>'.
    ENDIF.

    CONC_XML '</infCIOT>'.

  ENDIF.

  LOOP AT I_OBJ->AT_IT_DOC_MDFE  INTO I_OBJ->AT_WA_DOC_MDFE .

    IF VL_TP_DOC_REF NE '2'. "NF-e.

      CLEAR: WA_VTTK.
      ME->GET_DADOS_TRANSP( EXPORTING I_DOCNUM  = I_OBJ->AT_WA_DOC_MDFE-DOCNUM
                            RECEIVING E_ST_VTTK = WA_VTTK ).

    ENDIF.

    DO 4 TIMES.

      CLEAR: VL_PLACA.

      IF VL_TP_DOC_REF = '2'. "NF-e.
        CASE SY-INDEX.
          WHEN 1.
            I_OBJ->GET_PLACA_CAV(  RECEIVING E_PLACA_CAV  = VL_PLACA ).
          WHEN 2.
            I_OBJ->GET_PLACA_CAR1( RECEIVING E_PLACA_CAR1 = VL_PLACA ).
          WHEN 3.
            I_OBJ->GET_PLACA_CAR2( RECEIVING E_PLACA_CAR2 = VL_PLACA ).
          WHEN 4.
            I_OBJ->GET_PLACA_CAR3( RECEIVING E_PLACA_CAR3 = VL_PLACA ).
        ENDCASE.
      ELSE.
        CASE SY-INDEX.
          WHEN 1.
            VL_PLACA = WA_VTTK-TEXT1(7).
          WHEN 2.
            VL_PLACA = WA_VTTK-TEXT2(7).
          WHEN 3.
            VL_PLACA = WA_VTTK-TEXT3(7).
          WHEN 4.
            VL_PLACA = WA_VTTK-TEXT4(7).
        ENDCASE.
      ENDIF.

      "Selecionar os dados dos veículos.
      READ TABLE IT_ZLEST0002 WITH KEY PC_VEICULO = VL_PLACA TRANSPORTING NO FIELDS.
      IF SY-SUBRC IS INITIAL.
        SY-SUBRC = 4.
      ELSE.
        SELECT SINGLE * INTO ST_ZLEST0002 FROM ZLEST0002 WHERE PC_VEICULO = VL_PLACA.
      ENDIF.

      IF SY-SUBRC EQ 0.

        APPEND ST_ZLEST0002 TO IT_ZLEST0002.

        IF ( VINC_VEIC_TRACAO IS INITIAL ) OR
           ( ST_ZLEST0002-TP_VEICULO = '1' ). "Reboque

          IF ST_ZLEST0002-TP_VEICULO = '1'. "Reboque
            CONC_XML'<veicReboque>'.
          ELSE.
            VINC_VEIC_TRACAO = 'X'.
            CONC_XML'<veicTracao>'.  "Tração
          ENDIF.

          CONC_XML      '<placa>'.
          CONC_XML         ST_ZLEST0002-PC_VEICULO.
          CONC_XML      '</placa>'.

          CONC_XML      '<RENAVAM>'.
          CONC_XML         ST_ZLEST0002-CD_RENAVAM.
          CONC_XML      '</RENAVAM>'.

          CONC_XML      '<tara>'.
          CLEAR XVALOR.
          XVALOR = ST_ZLEST0002-TARA.
          CONC_XML        XVALOR.
          CONC_XML      '</tara>'.

          CONC_XML      '<capKG>'.
          CLEAR XVALOR.
          XVALOR = ST_ZLEST0002-CAP_KG.
          CONC_XML     XVALOR.
          CONC_XML      '</capKG>'.

          CONC_XML      '<capM3>'.
          CLEAR XVALOR.
          XVALOR = ST_ZLEST0002-CAP_M3.
          CONC_XML     XVALOR.
          CONC_XML      '</capM3>'.

          CONC_XML      '<tpRod>'.

          CLEAR: VL_TIPO_ROD.
          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
            EXPORTING
              INPUT  = ST_ZLEST0002-TP_RODADO
            IMPORTING
              OUTPUT = VL_TIPO_ROD.

          IF ( ST_ZLEST0002-TP_VEICULO EQ '0' ) AND "Tração
             ( VL_TIPO_ROD EQ '0' ).
            VL_TIPO_ROD = '3'.
          ENDIF.

          CONC_XML         VL_TIPO_ROD.

          CONC_XML      '</tpRod>'.

          CONC_XML      '<tpCar>'.

          CLEAR: VL_TIPO_CAR.
          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
            EXPORTING
              INPUT  = ST_ZLEST0002-TP_CARROCERIA2
            IMPORTING
              OUTPUT = VL_TIPO_CAR.

          CONC_XML         VL_TIPO_CAR.

          CONC_XML      '</tpCar>'.

          CONC_XML      '<UF>'.
          CONC_XML         ST_ZLEST0002-CD_UF.
          CONC_XML      '</UF>'.

          "Proprietários

          "Buscar os dados do parceiro PV (proprietário do veículo).
          CLEAR: ST_LFA1, ST_ADRC, ST_ADR6.

          IF VL_TP_DOC_REF = '2'. "NF-e.

            SELECT SINGLE L~NAME1 L~STCD1 L~STCD2 L~STCD3 L~BAHNS A~REGION L~ADRNR
              INTO ST_LFA1
              FROM LFA1 AS L INNER JOIN ADRC AS A ON L~ADRNR = A~ADDRNUMBER
             WHERE L~LIFNR = ST_ZLEST0002-PROPRIETARIO.

          ELSE.

            SELECT SINGLE L~NAME1 L~STCD1 L~STCD2 L~STCD3 L~BAHNS A~REGION L~ADRNR
              INTO ST_LFA1
              FROM VTPA AS V
                   INNER JOIN LFA1 AS L ON V~LIFNR = L~LIFNR
                   INNER JOIN ADRC AS A ON V~ADRNR = A~ADDRNUMBER
             WHERE VBELN = WA_VTTK-TKNUM
               AND PARVW = 'PV'.

          ENDIF.

          IF ( ST_LFA1 IS NOT INITIAL ).
            SELECT SINGLE *
              FROM ADRC INTO ST_ADRC
             WHERE ADDRNUMBER EQ ST_LFA1-ADRNR.

            SELECT SINGLE *
              FROM ADR6 INTO ST_ADR6
             WHERE ADDRNUMBER EQ ST_LFA1-ADRNR.
          ENDIF.

          IF ST_LFA1 IS NOT INITIAL.

            CONC_XML    '<prop>'.

            "CPF
            CLEAR VL_11POS.
            IF ST_LFA1-STCD2 IS NOT INITIAL.
              VL_11POS = ST_LFA1-STCD2.
              SHIFT VL_11POS RIGHT DELETING TRAILING ' '.
              OVERLAY VL_11POS WITH '00000000000'.
              CONC_XML      '<CPF>'.
              CONC_XML           VL_11POS.
              CONC_XML      '</CPF>'.
            ENDIF.

            "Informar o CNPJ se o CPF estiver em branco.
            CLEAR VL_14POS.
            IF ST_LFA1-STCD1 IS NOT INITIAL AND
               ST_LFA1-STCD2 IS INITIAL.
              VL_14POS = ST_LFA1-STCD1.
              SHIFT VL_14POS RIGHT DELETING TRAILING ' '.
              OVERLAY VL_14POS WITH '00000000000000'.
              CONC_XML      '<CNPJ>'.
              CONC_XML           VL_14POS.
              CONC_XML      '</CNPJ>'.
            ENDIF.

            CONC_XML        '<RNTRC>'.
            CONC_XML             ST_LFA1-BAHNS.
            CONC_XML        '</RNTRC>'.

            DATA(ST_LFA1_NAME1) = ZCL_STRING=>TIRA_ACENTOS( I_TEXTO = ZCL_STRING=>CONVERT_TO_UTF8( I_TEXTO = CONV #( ST_LFA1-NAME1 ) ) ).

            CONC_XML        '<xNome>'.
            CONC_XML             ST_LFA1_NAME1.
            CONC_XML        '</xNome>'.

            IF ( ST_LFA1-STCD3 IS NOT INITIAL ) AND ( ST_LFA1-STCD3 NE 'ISENTO' ).
              CONC_XML      '<IE>'.
              CONC_XML           ST_LFA1-STCD3.
              CONC_XML      '</IE>'.

              IF ( ST_LFA1-REGION IS NOT INITIAL ).
                CONC_XML    '<UF>'.
                CONC_XML         ST_LFA1-REGION.
                CONC_XML    '</UF>'.
              ENDIF.
            ENDIF.

            CONC_XML        '<tpProp>0</tpProp>'.

            IF ST_ADR6-SMTP_ADDR IS NOT INITIAL.

              DATA(ST_ADR6_SMTP_ADDR) = ZCL_STRING=>TIRA_ACENTOS( I_TEXTO = ZCL_STRING=>CONVERT_TO_UTF8( I_TEXTO = CONV #( ST_ADR6-SMTP_ADDR ) ) ).

              CONC_XML      '<email>'.
              CONC_XML         ST_ADR6_SMTP_ADDR.
              CONC_XML      '</email>'.
            ENDIF.

            CONC_XML    '</prop>'.

          ENDIF.

          IF ST_ZLEST0002-TP_VEICULO = '0'. "Tração

            " Buscar os dados do parceiro MT (motorista).
            CLEAR ST_LFA1.

            IF VL_TP_DOC_REF = '2'. "NF-e.

              I_OBJ->GET_MOTORISTA( RECEIVING E_MOTORISTA = VL_MOTORISTA ).

              SELECT SINGLE L~NAME1 L~STCD2 INTO (VL_NAME1, VL_STCD2)
                FROM LFA1 AS L
               WHERE L~LIFNR = VL_MOTORISTA.

            ELSE.

              SELECT SINGLE L~NAME1 L~STCD2 INTO (VL_NAME1, VL_STCD2)
                FROM VTPA AS V
                INNER JOIN LFA1 AS L ON V~LIFNR = L~LIFNR
                WHERE VBELN = WA_VTTK-TKNUM
                  AND PARVW = 'MT'.

            ENDIF.

            IF SY-SUBRC = 0.

              CONC_XML  '<condutor>'.

              VL_LIMPO = VL_NAME1.
              REPLACE ALL OCCURRENCES OF 'º' IN VL_LIMPO WITH 'o'.
              REPLACE ALL OCCURRENCES OF 'ª' IN VL_LIMPO WITH 'a'.

              VL_LIMPO = ZCL_STRING=>TIRA_ACENTOS( I_TEXTO = ZCL_STRING=>CONVERT_TO_UTF8( I_TEXTO = CONV #( VL_LIMPO ) ) ).

              CONC_XML      '<xNome>'.
              CONC_XML         VL_LIMPO.
              CONC_XML      '</xNome>'.

              CONC_XML      '<CPF>'.
              CONC_XML         VL_STCD2.
              CONC_XML      '</CPF>'.

              CONC_XML  '</condutor>'.
            ENDIF.

          ENDIF.

          IF ST_ZLEST0002-TP_VEICULO = '1'. "Reboque
            CONC_XML'</veicReboque>'.
          ELSE.
            CONC_XML'</veicTracao>'.  "Tração
          ENDIF.

        ENDIF.

      ENDIF.

    ENDDO.

  ENDLOOP.

  CLEAR: IT_ZLEST0002.

  SELECT SINGLE * INTO @DATA(WA_IDENTIFICA)
    FROM ZCTE_IDENTIFICA WHERE DOCNUM EQ @I_OBJ->AT_WA_DOC_MDFE-DOCNUM.

  SELECT SINGLE * INTO @DATA(WA_PARCEIRO)
    FROM ZCTE_PARCEIROS WHERE DOCNUM EQ @I_OBJ->AT_WA_DOC_MDFE-DOCNUM.

  CONC_XML '<infContratante>'.

  CASE WA_IDENTIFICA-TOMA.
    WHEN 0.

      IF WA_PARCEIRO-REME_CNPJ IS NOT INITIAL.
        CONC_XML '<CNPJ>'.
        CONC_XML WA_PARCEIRO-REME_CNPJ.
        CONC_XML '</CNPJ>'.
      ELSE.
        CONC_XML '<CPF>'.
        CONC_XML WA_PARCEIRO-REME_CPF.
        CONC_XML '</CPF>'.
      ENDIF.

    WHEN 3.

      IF WA_PARCEIRO-DEST_CNPJ IS NOT INITIAL.
        CONC_XML '<CNPJ>'.
        CONC_XML WA_PARCEIRO-DEST_CNPJ.
        CONC_XML '</CNPJ>'.
      ELSE.
        CONC_XML '<CPF>'.
        CONC_XML WA_PARCEIRO-DEST_CPF.
        CONC_XML '</CPF>'.
      ENDIF.

  ENDCASE.

  CONC_XML '</infContratante>'.


  CONC_XML '</rodo>'.



ENDMETHOD.
ENDCLASS.
