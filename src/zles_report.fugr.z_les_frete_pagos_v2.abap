FUNCTION Z_LES_FRETE_PAGOS_V2.
*"--------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(P_OPCAO) TYPE  CHAR10
*"  EXPORTING
*"     REFERENCE(IT_RESULTADO) TYPE  STANDARD TABLE
*"  TABLES
*"      IT_EMPRESA STRUCTURE  T001 OPTIONAL
*"      IT_DATA STRUCTURE  BSAD OPTIONAL
*"--------------------------------------------------------------------
*/===========================================================================\*
*|      db      `7MMM.     ,MMF'      db       .g8"""bgd    .g8"""bgd `7MMF' |*
*|     ;MM:       MMMb    dPMM       ;MM:    .dP'     `M  .dP'     `M   MM   |*
*|    ,V^MM.      M YM   ,M MM      ,V^MM.   dM'       `  dM'       `   MM   |*
*|   ,M  `MM      M  Mb  M' MM     ,M  `MM   MM           MM            MM   |*
*|   AbmmmqMA     M  YM.P'  MM     AbmmmqMA  MM.    `7MMF'MM.    `7MMF' MM   |*
*|  A'     VML    M  `YM'   MM    A'     VML `Mb.     MM  `Mb.     MM   MM   |*
*| AMA.   .AMMA..JML. `'  .JMML..AMA.   .AMMA. `"bmmmdPY    `"bmmmdPY .JMML. |*
*/===========================================================================\*

*/===========================================================================\*
*| Descrição:                                                                |*
*| Função para o report de frete pagos                                       |*
*| programa: ZLESR0026..                                                     |*
*/===========================================================================\*

*/===========================================================================\*
*|  Desenvolvedor:                                                           |*
*|    + Victor Hugo Souza Nunes ( victor.hugo@grupomaggi.com.br )            |*
*|                                                                           |*
*|  Tester:                                                                  |*
*|    + Marcos Santos ( marcos.santos@grupomaggi.com.br )                    |*
*|  Changelog:                                                               |*
*|                                                                           |*
*/===========================================================================\*

  DATA: V_LINES        TYPE I,
        V_DMBTR        TYPE ZLEST0105-DMBTR,
        V_DMBE2        TYPE ZLEST0105-DMBE2,
        V_EV_PRT_INI   TYPE BSAK-AUGDT,
        V_AQUAV_INI    TYPE BSAK-AUGDT,
        V_FPROP_INI    TYPE BSAK-AUGDT,
        V_VL_PAGO_LOTE TYPE ZLEST0105-VL_PAGO_LOTE.

  DATA: VL_TX_CAMBIO TYPE UKURS_CURR,
        VL_GDATU     TYPE GDATU_INV.

  DATA: IT_BRANCH_AUX TYPE TABLE OF J_1BBRANCH.

  DATA: OBJ_ZCL_UTIL_SD TYPE REF TO ZCL_UTIL_SD.

  CREATE OBJECT OBJ_ZCL_UTIL_SD.

  CLEAR: IT_BUKRS[], IT_AUGDT[], IT_GJAHR[].

  DESCRIBE TABLE IT_EMPRESA LINES VAR_LINHAS.

  CASE VAR_LINHAS.
    WHEN: '1'.
      CLEAR: GW_EMPRESA.
      READ TABLE IT_EMPRESA INTO GW_EMPRESA INDEX 1.
      IT_BUKRS-SIGN   = 'I'.
      IT_BUKRS-OPTION = 'EQ'.
      IT_BUKRS-LOW    = GW_EMPRESA-BUKRS.
      APPEND IT_BUKRS.
    WHEN: '2'.
      IT_BUKRS-SIGN   = 'I'.
      IT_BUKRS-OPTION = 'EQ'.
      READ TABLE IT_EMPRESA INTO GW_EMPRESA INDEX 1.
      IT_BUKRS-LOW    = GW_EMPRESA-BUKRS.
      CLEAR: GW_EMPRESA.
      READ TABLE IT_EMPRESA INTO GW_EMPRESA INDEX 2.
      IT_BUKRS-HIGH   = GW_EMPRESA-BUKRS.
      APPEND IT_BUKRS.
  ENDCASE.

  CLEAR: VAR_LINHAS.

  DESCRIBE TABLE IT_DATA LINES VAR_LINHAS.
  CASE VAR_LINHAS.
    WHEN: '1'.


      CLEAR: GW_DATA.
      READ TABLE IT_DATA INTO GW_DATA INDEX 1.
      IT_AUGDT-SIGN   = 'I'.
      IT_AUGDT-OPTION = 'EQ'.
      IT_AUGDT-LOW    = GW_DATA-AUGDT.
      APPEND IT_AUGDT.

      CLEAR: GW_GJAHR.

      IT_GJAHR-SIGN   = 'I'.
      IT_GJAHR-OPTION = 'EQ'.
      IT_GJAHR-LOW    = GW_DATA-AUGDT(4).

      APPEND IT_GJAHR.

      CLEAR: GW_GJAHR.

      IT_GJAHR-SIGN   = 'I'.
      IT_GJAHR-OPTION = 'EQ'.
      IT_GJAHR-LOW    = GW_DATA-AUGDT(4) - 1.

      APPEND IT_GJAHR.




    WHEN: '2'.

      IT_AUGDT-SIGN   = 'I'.
      IT_AUGDT-OPTION = 'BT'.
      CLEAR: GW_DATA.
      READ TABLE IT_DATA INTO GW_DATA INDEX 1.
      IT_AUGDT-LOW    = GW_DATA-AUGDT.

      CLEAR: GW_GJAHR.

      IT_GJAHR-SIGN   = 'I'.
      IT_GJAHR-OPTION = 'EQ'.
      IT_GJAHR-LOW    = GW_DATA-AUGDT(4).

      APPEND IT_GJAHR.

      CLEAR: GW_DATA.
      READ TABLE IT_DATA INTO GW_DATA INDEX 2.
      IT_AUGDT-HIGH   = GW_DATA-AUGDT.
      APPEND IT_AUGDT.

      CLEAR: GW_GJAHR.

      IT_GJAHR-SIGN   = 'I'.
      IT_GJAHR-OPTION = 'EQ'.
      IT_GJAHR-LOW    = GW_DATA-AUGDT(4).

      APPEND IT_GJAHR.

      CLEAR: GW_GJAHR.

      IT_GJAHR-SIGN   = 'I'.
      IT_GJAHR-OPTION = 'EQ'.
      IT_GJAHR-LOW    = GW_DATA-AUGDT(4) - 1.

      APPEND IT_GJAHR.




  ENDCASE.

  REFRESH: GT_SAIDA, GT_SAIDA_AUX, GT_ZLEST0105.
  CLEAR: GW_SAIDA, GW_SAIDA_AUX, GW_ZLEST0105.

  "CS2018000311 - 16.03.2018
  CLEAR: R_AUGGJ[], R_BSART[], R_BLART[], R_BLART1[], R_WAERS[], R_LIFNR[], R_LIFNR1[], R_LIFNR2[].

  CLEAR: TG_SETLEAF_ELEVA[].

  IF IT_AUGDT[] IS NOT INITIAL.
    R_AUGGJ-SIGN   = 'I'.
    R_AUGGJ-OPTION = 'EQ'.
    R_AUGGJ-LOW    = IT_AUGDT-LOW(4).
    R_AUGGJ-HIGH   = IT_AUGDT-LOW(4).
    APPEND R_AUGGJ.

    IF ( IT_AUGDT-HIGH(4) IS NOT INITIAL ) AND ( IT_AUGDT-HIGH(4) NE IT_AUGDT-LOW(4) ).
      R_AUGGJ-SIGN   = 'I'.
      R_AUGGJ-OPTION = 'EQ'.
      R_AUGGJ-LOW    = IT_AUGDT-HIGH(4).
      R_AUGGJ-HIGH   = IT_AUGDT-HIGH(4).
      APPEND R_AUGGJ.
    ENDIF.
  ENDIF.

  CASE P_OPCAO.
    WHEN 'R_AD_PI'.
      "Tipo Pedido
      R_BSART-SIGN   = 'I'.
      R_BSART-OPTION = 'EQ'.
      R_BSART-LOW    = 'ZFTE'.
      APPEND R_BSART.

      R_BSART-LOW    = 'ZSEM'.
      APPEND R_BSART.

      R_BSART-LOW    = 'ZDEF'.
      APPEND R_BSART.

      "Tipo Documento
      R_BLART-SIGN   = 'I'.
      R_BLART-OPTION = 'EQ'.
      R_BLART-LOW    = 'IN'.
      APPEND R_BLART.

      "Moeda
      R_WAERS-SIGN   = 'I'.
      R_WAERS-OPTION = 'EQ'.
      R_WAERS-LOW    = 'BRL'.
      APPEND R_WAERS.

    WHEN 'R_AQ_PA'.

      "Tipo Documento
      R_BLART-SIGN   = 'I'.
      R_BLART-OPTION = 'EQ'.
      R_BLART-LOW    = 'ME'.
      APPEND R_BLART.

      R_BLART-LOW    = 'MF'.
      APPEND R_BLART.

      "Moeda
      R_WAERS-SIGN   = 'I'.
      R_WAERS-OPTION = 'EQ'.
      R_WAERS-LOW    = 'BRL'.
      APPEND R_WAERS.

      "Fornecedor
      R_LIFNR-SIGN   = 'I'.
      R_LIFNR-OPTION = 'EQ'.
      R_LIFNR-LOW    = '0000000161'.
      APPEND R_LIFNR.

    WHEN 'R_EV_PRT'.

      SELECT *
        FROM SETLEAF APPENDING CORRESPONDING FIELDS OF TABLE TG_SETLEAF_ELEVA
       WHERE SETNAME = 'MAGGI_ZLES0079_ELEVA'.

      LOOP AT TG_SETLEAF_ELEVA WHERE VALFROM IS NOT INITIAL.

        SELECT SINGLE *
          FROM SETLINET INTO @DATA(_WL_SETLINET)
         WHERE SETNAME = @TG_SETLEAF_ELEVA-SETNAME
           AND LINEID  = @TG_SETLEAF_ELEVA-LINEID.

        IF ( SY-SUBRC = 0 ).
          TG_SETLEAF_ELEVA-DESCRIPT = _WL_SETLINET-DESCRIPT.
          MODIFY TG_SETLEAF_ELEVA.
        ENDIF.

        "Fornecedor
        R_LIFNR-SIGN   = 'I'.
        R_LIFNR-OPTION = 'EQ'.
        R_LIFNR-LOW    = TG_SETLEAF_ELEVA-VALFROM.
        APPEND R_LIFNR.

        IF TG_SETLEAF_ELEVA-DESCRIPT IS INITIAL.
          R_LIFNR1-SIGN   = 'I'.
          R_LIFNR1-OPTION = 'EQ'.
          R_LIFNR1-LOW    = TG_SETLEAF_ELEVA-VALFROM.
          APPEND R_LIFNR1.
        ELSE.
          R_LIFNR2-SIGN   = 'I'.
          R_LIFNR2-OPTION = 'EQ'.
          R_LIFNR2-LOW    = TG_SETLEAF_ELEVA-VALFROM.
          APPEND R_LIFNR2.
        ENDIF.

      ENDLOOP.

      IF R_LIFNR[] IS INITIAL.
        MESSAGE 'Nenhum fornecedor parametrizado para a opção de Elevação Portuária!' TYPE 'S'.
        EXIT.
      ENDIF.
    WHEN 'R_AQUAV'.

      "Tipo Documento
      R_BLART-SIGN   = 'I'.
      R_BLART-OPTION = 'EQ'.
      R_BLART-LOW    = 'MF'. APPEND R_BLART.
      R_BLART-LOW    = 'ME'. APPEND R_BLART.
      R_BLART-LOW    = 'FT'. APPEND R_BLART.

      "Fornecedor
      CLEAR: IT_BRANCH_AUX[].
      SELECT *
        FROM J_1BBRANCH INTO TABLE IT_BRANCH_AUX
       WHERE BUKRS  IN ( '0041' )
         AND BRANCH NE '0001'.

      R_LIFNR-SIGN   = 'I'.
      R_LIFNR-OPTION = 'EQ'.
      LOOP AT IT_BRANCH_AUX INTO DATA(WL_BRANCH_AUX).
        R_LIFNR-LOW  = WL_BRANCH_AUX-BRANCH .
        R_LIFNR-LOW  = |{ R_LIFNR-LOW ALPHA = IN } |.
        APPEND R_LIFNR.
      ENDLOOP.

      "Moeda
      R_WAERS-SIGN   = 'I'.
      R_WAERS-OPTION = 'EQ'.
      R_WAERS-LOW    = 'BRL'.
      APPEND R_WAERS.

    WHEN 'R_FPROP'.

    WHEN OTHERS.
  ENDCASE.
  "Fim CS2018000311 - 16.03.2018

  CASE P_OPCAO.
    WHEN: 'R_AD_CX'.
      "Perform Utilizado para selecionar informações do Adiantamento Caixa.
      "Não foi utilizado a seleção do perform SELECIONAR_DADOS porque este
      "fluxo é diferente, partindo primeiro na seleção de um SET e tornando
      "a tabela principal ZLEST0045.
      PERFORM: SELECIONAR_DADOS_ADT_CAIXA USING P_OPCAO.
    WHEN: 'R_AD_LQ'.
      PERFORM: SELECIONAR_DADOS_ADT_LQ.
    WHEN: 'R_AD_PI'.
      "PERFORM: SELECIONAR_DADOS_ADT_INSUMOS USING P_OPCAO.
      PERFORM: SELECIONAR_DADOS_ADT_N1 USING P_OPCAO,
               SELECIONAR_DADOS_JUROS USING P_OPCAO. "Seleção Juros
    WHEN: 'R_EV_PRT'.
      PERFORM: SELECIONAR_DADOS_ADT_N1 USING P_OPCAO,
               SELECIONAR_DADOS_JUROS  USING P_OPCAO. "Seleção Juros
    WHEN: 'R_COM'.
      PERFORM: SELECIONAR_DADOS_COMPENSADOS.
    WHEN: 'R_AQ_PA'.
      PERFORM: SELECIONAR_DADOS_ADT_N1 USING P_OPCAO,
               SELECIONAR_DADOS_JUROS  USING P_OPCAO. "Seleção Juros
    WHEN 'R_AQUAV'.
      PERFORM: SELECIONAR_DADOS        USING P_OPCAO ABAP_TRUE,
               SELECIONAR_DADOS_ADT_N1 USING P_OPCAO,
               SELECIONAR_DADOS_JUROS  USING P_OPCAO. "Seleção Juros

      DELETE GT_SAIDA WHERE LIFNR = '0000001003'.
      DELETE GT_SAIDA WHERE LIFNR = '0000001013'.
      DELETE GT_SAIDA WHERE LIFNR = '0000009402'.

    WHEN 'R_FPROP'.
      PERFORM: SELECIONAR_DADOS_FRETE_PROPRIO USING P_OPCAO.
    WHEN OTHERS.
      PERFORM SELECIONAR_DADOS USING P_OPCAO ABAP_FALSE.

  ENDCASE.

*-------------------------------------------------------------*
* Determinação Grupo de Contas
*-------------------------------------------------------------*
  CLEAR: GT_LFA1_KTOKK[].

  IF GT_SAIDA[] IS NOT INITIAL.
    SELECT *
      FROM LFA1 APPENDING TABLE GT_LFA1_KTOKK
       FOR ALL ENTRIES IN GT_SAIDA
     WHERE LIFNR EQ GT_SAIDA-LIFNR.
  ENDIF.

  LOOP AT GT_SAIDA ASSIGNING FIELD-SYMBOL(<FS_SAIDA_AUX>).
    READ TABLE GT_LFA1_KTOKK WITH KEY LIFNR = <FS_SAIDA_AUX>-LIFNR.
    CHECK SY-SUBRC EQ 0.
    <FS_SAIDA_AUX>-KTOKK = GT_LFA1_KTOKK-KTOKK.
  ENDLOOP.

  "------------------------------------------------------------------
  " Transferência p/ tabela ZLEST0105
  "------------------------------------------------------------------
  CLEAR: VAR_TIPO_REG.

  CASE P_OPCAO.
    WHEN: 'R_AD_CX'.
      VAR_TIPO_REG = 'FA'.
    WHEN: 'PAG'.
      VAR_TIPO_REG = 'FP'.
    WHEN 'R_AD_PI'.
      VAR_TIPO_REG = 'IA_IP'. "Tipo Definido no Registro
    WHEN 'R_EV_PRT'.
      VAR_TIPO_REG = 'EA_EP'. "Tipo Definido no Registro
    WHEN 'R_AQUAV'.
      VAR_TIPO_REG = 'AQ'.
    WHEN 'R_AQ_PA'.
      VAR_TIPO_REG = 'PA'.
    WHEN 'R_FPROP'.
      VAR_TIPO_REG = 'PR'.
  ENDCASE.

  "Lançamentos Manuais
  CLEAR: GT_ZLEST0105_MN[].
  IF VAR_TIPO_REG = 'IA_IP'.
    SELECT *
      FROM ZLEST0105 INTO TABLE GT_ZLEST0105_MN
     WHERE BUKRS      IN IT_BUKRS
       AND AUGDT      IN IT_AUGDT
       AND TIPO       IN ('IA','IP')
       AND INC_MANUAL EQ 'X'.
  ELSEIF VAR_TIPO_REG = 'EA_EP'.
    SELECT *
      FROM ZLEST0105 INTO TABLE GT_ZLEST0105_MN
     WHERE BUKRS      IN IT_BUKRS
       AND AUGDT      IN IT_AUGDT
       AND TIPO       IN ('EA','EP')
       AND INC_MANUAL EQ 'X'.
  ELSE.
    SELECT *
      FROM ZLEST0105 INTO TABLE GT_ZLEST0105_MN
     WHERE BUKRS   IN IT_BUKRS
       AND AUGDT   IN IT_AUGDT
       AND TIPO    EQ VAR_TIPO_REG
       AND INC_MANUAL EQ 'X'.
  ENDIF.

  IF P_OPCAO = 'R_AD_PI'.. "Insumos
    DELETE GT_SAIDA    WHERE AUGDT < '20170516'.
  ENDIF.

  IF P_OPCAO = 'R_EV_PRT'. "Elevação Portuaria
    SELECT SINGLE *
      FROM SETLEAF INTO @DATA(_WL_EV_PRT_INI)
     WHERE SETNAME = 'MAGGI_EV_PRT_INI'.

    IF ( SY-SUBRC EQ 0 ) AND ( _WL_EV_PRT_INI-VALFROM IS NOT INITIAL ).
      V_EV_PRT_INI = _WL_EV_PRT_INI-VALFROM.
      DELETE GT_SAIDA    WHERE AUGDT < V_EV_PRT_INI.
    ENDIF.
  ENDIF.

  IF ( P_OPCAO EQ 'R_AQUAV' ) OR
     ( P_OPCAO EQ 'R_AQ_PA' ).
    SELECT SINGLE *
      FROM SETLEAF INTO @DATA(_WL_ZLES0079_AQUAV_INI)
     WHERE SETNAME = 'ZLES0079_AQUAV_INI'.

    IF ( SY-SUBRC EQ 0 ) AND ( _WL_ZLES0079_AQUAV_INI-VALFROM IS NOT INITIAL ).
      V_AQUAV_INI  = _WL_ZLES0079_AQUAV_INI-VALFROM.
      DELETE GT_SAIDA WHERE AUGDT < V_AQUAV_INI.
    ENDIF.
  ENDIF.

  IF ( P_OPCAO EQ 'R_FPROP' ).
    SELECT SINGLE *
      FROM SETLEAF INTO @DATA(_WL_ZLES0079_FPROP_INI)
     WHERE SETNAME = 'ZLES0079_FPROP_INI'.

    IF ( SY-SUBRC EQ 0 ) AND ( _WL_ZLES0079_FPROP_INI-VALFROM IS NOT INITIAL ).
      V_FPROP_INI  = _WL_ZLES0079_FPROP_INI-VALFROM.
      DELETE GT_SAIDA WHERE AUGDT < V_FPROP_INI.
    ELSE.
      CLEAR: GT_SAIDA[].
    ENDIF.
  ENDIF.

*------------------------------------------------*
* Tratamento Lançamentos Empresa 0032
*------------------------------------------------*
  RANGES: R_MATKL_GRAOS FOR MARA-MATKL.

  CLEAR: R_MATKL_GRAOS[].

  APPEND VALUE #( SIGN = 'I' OPTION = 'EQ' LOW = '700110' ) TO R_MATKL_GRAOS.
  APPEND VALUE #( SIGN = 'I' OPTION = 'EQ' LOW = '700170' ) TO R_MATKL_GRAOS.
  APPEND VALUE #( SIGN = 'I' OPTION = 'EQ' LOW = ''       ) TO R_MATKL_GRAOS.

  DELETE GT_SAIDA WHERE BUKRS = '0032' AND ( KTOKK EQ 'ZFIC' OR MATKL NOT IN R_MATKL_GRAOS ).

  "CHECK NOT GT_SAIDA[] IS INITIAL OR  GT_ZLEST0105_MN[] IS NOT INITIAL.

  CLEAR: GT_BKPF_GRV[], GT_VTTK_GRV[], GT_TVTKT_GRV[], GT_J_1BNFDOC_GRV[].
  IF GT_SAIDA[] IS NOT INITIAL.
    SELECT *
      FROM BKPF APPENDING CORRESPONDING FIELDS OF TABLE GT_BKPF_GRV
       FOR ALL ENTRIES IN GT_SAIDA
     WHERE BUKRS EQ GT_SAIDA-BUKRS
       AND BELNR EQ GT_SAIDA-BELNR.

    SELECT *
      FROM VTTK APPENDING CORRESPONDING FIELDS OF TABLE GT_VTTK_GRV
       FOR ALL ENTRIES IN GT_SAIDA
     WHERE TKNUM EQ GT_SAIDA-TKNUM.

    SELECT *
      FROM J_1BNFDOC INTO TABLE GT_J_1BNFDOC_GRV
       FOR ALL ENTRIES IN GT_SAIDA
     WHERE DOCNUM EQ GT_SAIDA-DOCNUM.
  ENDIF.

  IF GT_ZLEST0105_MN[] IS NOT INITIAL.
    SELECT *
      FROM BKPF APPENDING CORRESPONDING FIELDS OF TABLE GT_BKPF_GRV
       FOR ALL ENTRIES IN GT_ZLEST0105_MN
     WHERE BUKRS EQ GT_ZLEST0105_MN-BUKRS
       AND BELNR EQ GT_ZLEST0105_MN-BELNR.

    SELECT *
      FROM VTTK APPENDING CORRESPONDING FIELDS OF TABLE GT_VTTK_GRV
       FOR ALL ENTRIES IN GT_ZLEST0105_MN
     WHERE TKNUM EQ GT_ZLEST0105_MN-TKNUM.
  ENDIF.

  IF GT_VTTK_GRV[] IS NOT INITIAL.
    SELECT *
      FROM TVTKT INTO TABLE GT_TVTKT_GRV
       FOR ALL ENTRIES IN GT_VTTK_GRV
     WHERE SPRAS = SY-LANGU
       AND SHTYP = GT_VTTK_GRV-SHTYP.
  ENDIF.

  LOOP AT GT_SAIDA INTO GW_SAIDA.

    DATA(_TABIX) = SY-TABIX.
    DATA(_DEL)   = ABAP_FALSE.

    CASE VAR_TIPO_REG.
      WHEN 'EA_EP'. "Elevação Portuária.

        READ TABLE TG_SETLEAF_ELEVA WITH KEY VALFROM = GW_SAIDA-LIFNR.
        IF ( SY-SUBRC EQ 0 ).
          CASE TG_SETLEAF_ELEVA-DESCRIPT(1).
            WHEN ABAP_TRUE.
              READ TABLE GT_BKPF_GRV WITH KEY BUKRS = GW_SAIDA-BUKRS
                                              BELNR = GW_SAIDA-BELNR.
              IF SY-SUBRC EQ 0.
                GW_SAIDA-ZFBDT = GT_BKPF_GRV-BLDAT.
                GW_SAIDA-AUGDT = GT_BKPF_GRV-BLDAT.

                CLEAR: VL_TX_CAMBIO.
                MOVE GW_SAIDA-AUGDT TO VL_GDATU.

                OBJ_ZCL_UTIL_SD->SET_KURST('B').
                OBJ_ZCL_UTIL_SD->SET_WAERK('USD').
                OBJ_ZCL_UTIL_SD->SET_TCURR('BRL').
                OBJ_ZCL_UTIL_SD->SET_DATA( VL_GDATU ).

                VL_TX_CAMBIO = ABS( OBJ_ZCL_UTIL_SD->TAXA_CAMBIO( ) ).

                IF VL_TX_CAMBIO > 0.
                  IF GT_BKPF_GRV-WAERS EQ 'BRL'.
                    GW_SAIDA-DMBE2   = GW_SAIDA-DMBTR / VL_TX_CAMBIO.
                  ELSE.
                    GW_SAIDA-DMBTR   = GW_SAIDA-DMBE2 * VL_TX_CAMBIO.
                  ENDIF.
                  GW_SAIDA-TX_CAMB = VL_TX_CAMBIO.
                ENDIF.

              ENDIF.
          ENDCASE.
        ENDIF.

        IF GW_SAIDA-AUGDT NOT IN IT_AUGDT.
          _DEL = ABAP_TRUE.
        ENDIF.

        IF V_EV_PRT_INI IS NOT INITIAL.
          IF GW_SAIDA-AUGDT < V_EV_PRT_INI.
            _DEL = ABAP_TRUE.
          ENDIF.
        ENDIF.

      WHEN OTHERS.
    ENDCASE.

    IF _DEL EQ ABAP_TRUE.
      DELETE GT_SAIDA INDEX _TABIX.
    ELSE.
      MODIFY GT_SAIDA FROM GW_SAIDA INDEX _TABIX.
    ENDIF.

  ENDLOOP.

  IF ( VAR_TIPO_REG IS NOT INITIAL ).

    "Deletar Lançamentos para refazer tabela.
    IF ( IT_BUKRS IS NOT INITIAL ) AND ( IT_AUGDT IS NOT INITIAL ).

      DATA: VL_DEL_0105 TYPE C.

      CLEAR: GT_ZLEST0105_AUX[].

      IF VAR_TIPO_REG = 'IA_IP'.
        SELECT *
          FROM ZLEST0105 INTO TABLE GT_ZLEST0105_AUX
         WHERE BUKRS   IN IT_BUKRS
           AND AUGDT   IN IT_AUGDT
           AND TIPO    IN ('IA','IP')
           AND INC_MANUAL EQ ''.
      ELSEIF VAR_TIPO_REG = 'EA_EP'.
        SELECT *
          FROM ZLEST0105 INTO TABLE GT_ZLEST0105_AUX
         WHERE BUKRS   IN IT_BUKRS
           AND AUGDT   IN IT_AUGDT
           AND TIPO    IN ('EA','EP')
           AND INC_MANUAL EQ ''.

        IF GT_SAIDA[] IS NOT INITIAL.
          SELECT *
            FROM ZLEST0105 APPENDING CORRESPONDING FIELDS OF TABLE GT_ZLEST0105_AUX
             FOR ALL ENTRIES IN GT_SAIDA
           WHERE BUKRS EQ GT_SAIDA-BUKRS
             AND BELNR EQ GT_SAIDA-BELNR.
        ENDIF.
      ELSE.
        SELECT *
          FROM ZLEST0105 INTO TABLE GT_ZLEST0105_AUX
         WHERE BUKRS   IN IT_BUKRS
           AND AUGDT   IN IT_AUGDT
           AND TIPO    EQ VAR_TIPO_REG
           AND INC_MANUAL EQ ''.
      ENDIF.

      SORT GT_ZLEST0105_AUX BY BUKRS BELNR AUGBL DOCNUM TKNUM CHVID TIPO ESTORNO.

      LOOP AT GT_ZLEST0105_AUX INTO GW_ZLEST0105_AUX WHERE ESTORNO = ''.

        CLEAR: VL_DEL_0105.

        READ TABLE GT_ZLEST0105_AUX INTO GW_ZLEST0105 WITH KEY BUKRS    = GW_ZLEST0105_AUX-BUKRS
                                                               BELNR    = GW_ZLEST0105_AUX-BELNR
                                                               AUGBL    = GW_ZLEST0105_AUX-AUGBL
                                                               DOCNUM   = GW_ZLEST0105_AUX-DOCNUM
                                                               TKNUM    = GW_ZLEST0105_AUX-TKNUM
                                                               CHVID    = GW_ZLEST0105_AUX-CHVID
                                                               TIPO     = GW_ZLEST0105_AUX-TIPO
                                                               ESTORNO  = 'X'
                                                               BINARY SEARCH.
        CASE SY-SUBRC.
          WHEN 0.
            IF ( GW_ZLEST0105-ESTORNO_MANUAL IS INITIAL ).
              VL_DEL_0105 = 'X'.
            ENDIF.
          WHEN OTHERS.
            VL_DEL_0105 = 'X'.
        ENDCASE.

        IF VL_DEL_0105 IS NOT INITIAL.
          DELETE FROM ZLEST0105 WHERE BUKRS          = GW_ZLEST0105_AUX-BUKRS
                                  AND BELNR          = GW_ZLEST0105_AUX-BELNR
                                  AND AUGBL          = GW_ZLEST0105_AUX-AUGBL
                                  AND DOCNUM         = GW_ZLEST0105_AUX-DOCNUM
                                  AND TKNUM          = GW_ZLEST0105_AUX-TKNUM
                                  AND CHVID          = GW_ZLEST0105_AUX-CHVID
                                  AND TIPO           = GW_ZLEST0105_AUX-TIPO
                                  AND ESTORNO_MANUAL = ''.
        ENDIF.

      ENDLOOP.

      "COMMIT WORK.
    ENDIF.
    "Fim - Refazer Lançamentos ZLEST015para refazer tabela.

    "Busca registros das Tables Standard que não existem na ZLEST0105
    LOOP AT GT_SAIDA INTO GW_SAIDA.

      "Se tiver modificação/inclusão manual, não prosseguir
      READ TABLE GT_ZLEST0105_MN WITH KEY BUKRS      = GW_SAIDA-BUKRS
                                          BELNR      = GW_SAIDA-BELNR
                                          AUGBL      = GW_SAIDA-AUGBL
                                          DOCNUM     = GW_SAIDA-DOCNUM
                                          TKNUM      = GW_SAIDA-TKNUM
                                          XBLNR      = GW_SAIDA-XBLNR
                                          CHVID      = GW_SAIDA-CHVID
                                          TIPO       = GW_SAIDA-TIPO.
      CHECK SY-SUBRC NE 0.

      IF GW_SAIDA-XBLNR EQ 'ESTORNO'.
        SELECT SINGLE *
          INTO GW_ZLEST0105
          FROM ZLEST0105
         WHERE BELNR   = GW_SAIDA-BELNR
           AND AUGBL   = GW_SAIDA-AUGBL
           AND BUKRS   = GW_SAIDA-BUKRS
           AND DOCNUM  = GW_SAIDA-DOCNUM
           AND TKNUM   = GW_SAIDA-TKNUM
           AND XBLNR   = GW_SAIDA-XBLNR
           AND CHVID   = GW_SAIDA-CHVID
           AND TIPO    = GW_SAIDA-TIPO
           AND INC_MANUAL = ''
           AND ESTORNO = 'X'.
      ELSE.
        SELECT SINGLE *
          INTO GW_ZLEST0105
          FROM ZLEST0105
          WHERE BELNR   = GW_SAIDA-BELNR
            AND AUGBL   = GW_SAIDA-AUGBL
            AND BUKRS   = GW_SAIDA-BUKRS
            AND DOCNUM  = GW_SAIDA-DOCNUM
            AND TKNUM   = GW_SAIDA-TKNUM
            AND XBLNR   = GW_SAIDA-XBLNR
            AND CHVID   = GW_SAIDA-CHVID
            AND INC_MANUAL = ''
            AND TIPO    = GW_SAIDA-TIPO.
      ENDIF.

      IF SY-SUBRC <> 0.

        CLEAR: GW_ZLEST0105.

        MOVE-CORRESPONDING GW_SAIDA TO GW_ZLEST0105.

        GW_ZLEST0105-US_PROC  = SY-UNAME.
        GW_ZLEST0105-DT_ATUAL = SY-DATUM.
        GW_ZLEST0105-HR_ATUAL = SY-UZEIT.

        IF GW_SAIDA-XBLNR EQ 'ESTORNO'.
          GW_ZLEST0105-ESTORNO = 'X'.
        ENDIF.

        APPEND GW_ZLEST0105 TO GT_ZLEST0105.

        INSERT ZLEST0105 FROM GW_ZLEST0105.
        "COMMIT WORK.

      ELSE.
        CLEAR: GW_ZLEST0105.

        MOVE-CORRESPONDING GW_SAIDA TO GW_ZLEST0105.
        APPEND GW_ZLEST0105 TO GT_ZLEST0105.
      ENDIF.

      "Agrupar Partidas do documento.
      CLEAR: V_LINES, V_DMBTR, V_DMBE2, V_VL_PAGO_LOTE.

      LOOP AT GT_ZLEST0105 INTO DATA(_WL_0105) WHERE BUKRS      = GW_ZLEST0105-BUKRS
                                                 AND BELNR      = GW_ZLEST0105-BELNR
                                                 AND AUGBL      = GW_ZLEST0105-AUGBL
                                                 AND DOCNUM     = GW_ZLEST0105-DOCNUM
                                                 AND TKNUM      = GW_ZLEST0105-TKNUM
                                                 AND XBLNR      = GW_ZLEST0105-XBLNR
                                                 AND CHVID      = GW_ZLEST0105-CHVID
                                                 AND TIPO       = GW_ZLEST0105-TIPO
                                                 AND ESTORNO    = GW_ZLEST0105-ESTORNO.

        ADD 1 TO V_LINES.
        ADD _WL_0105-DMBTR        TO V_DMBTR.
        ADD _WL_0105-DMBE2        TO V_DMBE2.
        ADD _WL_0105-VL_PAGO_LOTE TO V_VL_PAGO_LOTE.
      ENDLOOP.

      IF V_LINES > 1.
        GW_ZLEST0105-DMBTR        = V_DMBTR.
        GW_ZLEST0105-DMBE2        = V_DMBE2.
        GW_ZLEST0105-VL_PAGO_LOTE = V_VL_PAGO_LOTE.
        MODIFY ZLEST0105 FROM GW_ZLEST0105.
      ENDIF.
      "Fim Agrupamento

    ENDLOOP.

    "Transferência Lancamentos Manuais
    LOOP AT GT_ZLEST0105_MN.

      DELETE GT_ZLEST0105 WHERE BUKRS      = GT_ZLEST0105_MN-BUKRS
                            AND BELNR      = GT_ZLEST0105_MN-BELNR
                            AND AUGBL      = GT_ZLEST0105_MN-AUGBL
                            AND DOCNUM     = GT_ZLEST0105_MN-DOCNUM
                            AND TKNUM      = GT_ZLEST0105_MN-TKNUM
                            AND XBLNR      = GT_ZLEST0105_MN-XBLNR
                            AND CHVID      = GT_ZLEST0105_MN-CHVID
                            AND TIPO       = GT_ZLEST0105_MN-TIPO
                            AND ESTORNO    = GT_ZLEST0105_MN-ESTORNO.

      CLEAR: GW_ZLEST0105.
      MOVE-CORRESPONDING GT_ZLEST0105_MN TO GW_ZLEST0105.
      APPEND GW_ZLEST0105 TO GT_ZLEST0105.

      "Remover estorno de lançamentos manuais.
      DELETE FROM ZLEST0105 WHERE BUKRS    = GT_ZLEST0105_MN-BUKRS
                              AND BELNR    = GT_ZLEST0105_MN-BELNR
                              AND AUGBL    = GT_ZLEST0105_MN-AUGBL
                              AND DOCNUM   = GT_ZLEST0105_MN-DOCNUM
                              AND TKNUM    = GT_ZLEST0105_MN-TKNUM
                              AND CHVID    = GT_ZLEST0105_MN-CHVID
                              AND TIPO     = GT_ZLEST0105_MN-TIPO
                              AND ESTORNO  = 'X'.

    ENDLOOP.

    "Copia Dados tabela Z p/ Tabela Aux.
    REFRESH: GT_SAIDA_AUX.
    CLEAR: GW_SAIDA_AUX, GW_ZLEST0105.

    LOOP AT GT_ZLEST0105 INTO GW_ZLEST0105.

      MOVE-CORRESPONDING GW_ZLEST0105 TO GW_SAIDA_AUX.
      GW_SAIDA_AUX-EXC_EST = ICON_EXECUTE_OBJECT.

      CLEAR: GW_LFA1.
      IF GW_SAIDA_AUX-LIFNR IS NOT INITIAL.
        SELECT SINGLE *
          INTO GW_LFA1
          FROM LFA1
         WHERE LIFNR = GW_SAIDA_AUX-LIFNR.

        IF SY-SUBRC EQ 0.
          GW_SAIDA_AUX-NAME1 = GW_LFA1-NAME1.
        ENDIF.
      ENDIF.

      IF GW_SAIDA_AUX-TKNUM IS NOT INITIAL.
        READ TABLE GT_VTTK_GRV WITH KEY TKNUM = GW_SAIDA_AUX-TKNUM.
        IF SY-SUBRC EQ 0.
          READ TABLE GT_TVTKT_GRV WITH KEY SHTYP = GT_VTTK_GRV-SHTYP.
          IF SY-SUBRC EQ 0.
            GW_SAIDA_AUX-BEZEI = GT_TVTKT_GRV-SHTYP && '-' && GT_TVTKT_GRV-BEZEI.
          ENDIF.
        ENDIF.
      ENDIF.

      IF GW_SAIDA_AUX-DOCNUM IS NOT INITIAL.
        READ TABLE GT_J_1BNFDOC_GRV WITH KEY DOCNUM = GW_SAIDA_AUX-DOCNUM.
        IF SY-SUBRC EQ 0.
          GW_SAIDA_AUX-CTENUM = GT_J_1BNFDOC_GRV-NFENUM.
        ENDIF.
      ENDIF.

      CASE GW_SAIDA_AUX-ADD03.
        WHEN 'Proprio'.
          GW_SAIDA_AUX-TP_FRETE = 'P'.
        WHEN 'Terceiro'.
          GW_SAIDA_AUX-TP_FRETE = 'T'.
      ENDCASE.

      APPEND GW_SAIDA_AUX TO GT_SAIDA_AUX.

      CLEAR: GW_SAIDA_AUX, GW_ZLEST0105.

    ENDLOOP.

  ENDIF.

  COMMIT WORK.

  "Busca lançamentos ZLEST0105 que foram estornardos nas tabelas standard, para atualização
*  REFRESH: GT_ZLEST0105.
*  SELECT *
*    FROM ZLEST0105 AS A INTO TABLE GT_ZLEST0105
*   WHERE A~BUKRS   IN IT_BUKRS
*     AND A~AUGDT   IN IT_AUGDT
*     AND A~TIPO    EQ VAR_TIPO_REG
*     AND A~ESTORNO EQ ''
*     AND NOT EXISTS ( SELECT *
*                       FROM ZLEST0105 AS B
*                      WHERE B~BUKRS   = A~BUKRS
*                        AND B~BELNR   = A~BELNR
*                        AND B~AUGBL   = A~AUGBL
*                        AND B~TIPO    = A~TIPO
*                        AND B~ESTORNO NE '' ).
*
*  LOOP AT GT_ZLEST0105 INTO GW_ZLEST0105.
*    CLEAR: VAR_DOC_ESTORNO.
*
*    IF ( GW_ZLEST0105-BELNR IS NOT INITIAL ) AND
*       ( GW_ZLEST0105-BUKRS IS NOT INITIAL ) AND
*       ( GW_ZLEST0105-BUDAT IS NOT INITIAL ).
*
*      CLEAR: GW_BKPF.
*      SELECT SINGLE *
*        FROM BKPF INTO CORRESPONDING FIELDS OF GW_BKPF
*       WHERE BELNR EQ GW_ZLEST0105-BELNR
*         AND BUKRS EQ GW_ZLEST0105-BUKRS
*         AND GJAHR EQ GW_ZLEST0105-BUDAT(4)
*         AND STBLG EQ ''.
*
*      IF SY-SUBRC NE 0.
*        VAR_DOC_ESTORNO = 'X'.
*      ENDIF.
*    ENDIF.
*
*    IF ( GW_ZLEST0105-AUGBL IS NOT INITIAL ) AND
*       ( GW_ZLEST0105-BUKRS IS NOT INITIAL ) AND
*       ( GW_ZLEST0105-AUGDT IS NOT INITIAL ) AND
*       ( VAR_DOC_ESTORNO IS INITIAL ).
*
*      CLEAR: GW_BKPF.
*      SELECT SINGLE *
*        FROM BKPF INTO CORRESPONDING FIELDS OF GW_BKPF
*       WHERE BELNR EQ GW_ZLEST0105-AUGBL
*         AND BUKRS EQ GW_ZLEST0105-BUKRS
*         AND GJAHR EQ GW_ZLEST0105-AUGDT(4)
*         AND STBLG EQ ''.
*
*      IF SY-SUBRC NE 0.
*        VAR_DOC_ESTORNO = 'X'.
*      ENDIF.
*    ENDIF.
*
*    IF VAR_DOC_ESTORNO IS NOT INITIAL.
*      GW_ZLEST0105-US_PROC  = SY-UNAME.
*      GW_ZLEST0105-DT_ATUAL = SY-DATUM.
*      GW_ZLEST0105-HR_ATUAL = SY-UZEIT.
*      GW_ZLEST0105-XBLNR    = 'ESTORNO'.
*      GW_ZLEST0105-DMBTR    = GW_ZLEST0105-DMBTR * -1.
*      GW_ZLEST0105-DMBE2    = GW_ZLEST0105-DMBE2 * -1.
*      GW_ZLEST0105-VL_PAGO_LOTE = GW_ZLEST0105-VL_PAGO_LOTE * -1.
*      GW_ZLEST0105-ESTORNO  = 'X'.
*      INSERT ZLEST0105 FROM GW_ZLEST0105.
*      COMMIT WORK.
*    ENDIF.
*  ENDLOOP.

  "------------------------------------------------------------------
  " Transferência Fim
  "------------------------------------------------------------------
  IF ( VAR_TIPO_REG IS NOT INITIAL ).
    IT_RESULTADO[] = GT_SAIDA_AUX[].
  ELSE.
    IT_RESULTADO[] = GT_SAIDA[].
  ENDIF.

ENDFUNCTION.
