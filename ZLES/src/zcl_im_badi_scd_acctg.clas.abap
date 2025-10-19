class ZCL_IM_BADI_SCD_ACCTG definition
  public
  final
  create public .

*"* public components of class ZCL_IM_BADI_SCD_ACCTG
*"* do not include other source files here!!!
public section.

  interfaces IF_EX_BADI_SCD_ACCTG .
protected section.
*"* protected components of class ZCL_IM_BADI_SCD_ACCTG
*"* do not include other source files here!!!
private section.
*"* private components of class ZCL_IM_BADI_SCD_ACCTG
*"* do not include other source files here!!!
ENDCLASS.



CLASS ZCL_IM_BADI_SCD_ACCTG IMPLEMENTATION.


METHOD IF_EX_BADI_SCD_ACCTG~BEFORE_CHECK.

  DATA: BEGIN OF WK_VTTK,
          TKNUM TYPE VTTK-TKNUM,
          SHTYP TYPE VTTK-SHTYP,
          ADD03 TYPE VTTK-ADD03,
          TDLNR TYPE VTTK-TDLNR,
          TEXT1 TYPE VTTK-TEXT1,
        END OF WK_VTTK,

        WK_FATURA TYPE ZLEST0021-FATURA.

* Determinação do tipo de documento de transporte
  SELECT SINGLE TKNUM SHTYP ADD03 TDLNR TEXT1
    INTO WK_VTTK
    FROM VTTK
   WHERE TKNUM = I_SCD_ITEM-VFKP-REBEL.

  IF SY-SUBRC IS INITIAL.

    IF WK_VTTK-ADD03 = '0000000001'.
      WK_FATURA = 'P'.
    ELSE.
      WK_FATURA = 'T'.
    ENDIF.

    DATA: V_VBELN    TYPE VTTP-VBELN,
          V_WERKS    TYPE LIPS-WERKS,
          V_BRANCH   TYPE J_1BBRANCH-BRANCH,
          V_BUKRS1   TYPE J_1BBRANCH-BUKRS,
          V_BUKRS2   TYPE J_1BBRANCH-BUKRS,
          WK_EMISSOR TYPE ZLEST0021-TP_EMISSOR.

    DATA: RGVEICU TYPE RANGE OF ZDE_TP_PROP_VEICULO_CTB.

    WK_EMISSOR = 'T'.
    IF WK_VTTK-ADD03 = '0000000001'. "Frete Próprio

      TRY .

          ZCL_FATURAMENTO=>ZIF_FATURAMENTO~GET_INSTANCE(
            )->GET_TIPO_VEICULO(
            EXPORTING
              I_PLACA          = WK_VTTK-TEXT1(7)    " Placa do Veículo
            IMPORTING
              E_TIPO           = DATA(E_TIPO)       " Tipo de Propriedade de veículo
              E_PROPRIETARIO   = DATA(E_PROPRIETARIO)     " Mestre de fornecedores (parte geral)
          ).

          IF E_TIPO NE ZIF_FATURAMENTO=>ST_TP_PROP_VEICULO_PROPRIO.
            RGVEICU = VALUE #( SIGN = 'I' OPTION = 'EQ' ( LOW = SPACE HIGH = SPACE ) ( LOW = '0' HIGH = '0' ) ).
          ELSE.

            DATA(E_AGENTE_FRETE) = CAST ZCL_FORNECEDORES(
            ZCL_FORNECEDORES=>ZIF_PARCEIROS~GET_INSTANCE(
              )->SET_PARCEIRO( I_PARCEIRO = WK_VTTK-TDLNR
              ) )->AT_LFA1.

            IF E_AGENTE_FRETE-STCD1(8) EQ E_PROPRIETARIO-STCD1(8) AND E_PROPRIETARIO-STCD1 IS NOT INITIAL.
              RGVEICU = VALUE #( SIGN = 'I' OPTION = 'EQ' ( LOW = '1' HIGH = '1' ) ).
            ELSE.
              RGVEICU = VALUE #( SIGN = 'I' OPTION = 'EQ' ( LOW = '2' HIGH = '2' ) ).
            ENDIF.

          ENDIF.

        CATCH ZCX_FATURAMENTO.    "
          RGVEICU = VALUE #( SIGN = 'I' OPTION = 'EQ' ( LOW = SPACE HIGH = SPACE ) ( LOW = '0' HIGH = '0' ) ).
        CATCH ZCX_ERROR.    " .
          RGVEICU = VALUE #( SIGN = 'I' OPTION = 'EQ' ( LOW = SPACE HIGH = SPACE ) ( LOW = '0' HIGH = '0' ) ).
      ENDTRY.

      WK_EMISSOR = 'P'.
      SELECT SINGLE VBELN FROM VTTP INTO V_VBELN WHERE TKNUM = WK_VTTK-TKNUM.
      IF SY-SUBRC = 0.
        SELECT SINGLE WERKS FROM LIPS INTO V_WERKS WHERE VBELN = V_VBELN.
        IF SY-SUBRC = 0.
          SELECT SINGLE BUKRS FROM J_1BBRANCH INTO V_BUKRS1 WHERE BRANCH = V_WERKS.
          IF SY-SUBRC = 0.
            CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
              EXPORTING
                INPUT  = WK_VTTK-TDLNR
              IMPORTING
                OUTPUT = V_BRANCH.

            CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
              EXPORTING
                INPUT  = V_BRANCH
              IMPORTING
                OUTPUT = V_BRANCH.
            SELECT SINGLE BUKRS FROM J_1BBRANCH INTO V_BUKRS2 WHERE BRANCH = V_BRANCH.
            IF SY-SUBRC = 0.
              IF V_BUKRS1 NE V_BUKRS2.
                WK_EMISSOR = 'I'. " PROPRIO INTERCOMPANY
              ENDIF.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
    ELSE.
      RGVEICU = VALUE #( SIGN = 'I' OPTION = 'EQ' ( LOW = SPACE HIGH = SPACE ) ( LOW = '0' HIGH = '0' ) ).
    ENDIF.

    TRY .

        ZCL_CONTROLE_CONTA_RAZAO=>GET_INSTANCE(
          )->GET_CONTA_RAZAO(
          EXPORTING
            I_SHTYP                  = WK_VTTK-SHTYP " Tipo de transporte
            I_TCODE                  = 'ML81N'       " Código de transação
            I_FATURA                 = WK_FATURA     " Emissor da fatura - Fretes
            I_TP_EMISSOR             = WK_EMISSOR    " Tipo de emissor
            I_OPERFRETE              = '15'          " Operação de lançamento no razão - Frete
            I_TP_VEICULO             = RGVEICU       " Tipo de Proprietário de Veículo para Contabilização
            I_DT_REFERENCIA          = I_SCD_ITEM-VFKP-BUDAT  " Data de lançamento no documento
          IMPORTING
            E_ZLEST0021              = DATA(E_ZLEST0021)    " Controle de desterminação conta razão
        ).
      CATCH ZCX_CONTROLE_CONTA_RAZAO.    "
    ENDTRY.

    C_VFKN-SAKTO = E_ZLEST0021-RAZAODEB.

*    SELECT SINGLE RAZAODEB
*      INTO C_VFKN-SAKTO      FROM ZLEST0021
*     WHERE SHTYP      EQ WK_VTTK-SHTYP
*       AND TCODE      EQ 'ML81N'
*       AND FATURA     EQ WK_FATURA
*       AND TP_EMISSOR EQ WK_EMISSOR
*       AND OPERFRETE  EQ '15'
*       AND TP_VEICULO IN RGVEICU.

* EXPORT wk_vttk TO MEMORY ID 'CUSTOFRETE'.
    EXPORT WK_VTTK FROM WK_VTTK TO MEMORY ID 'CUSTOFRETE'.

  ENDIF.

ENDMETHOD.
ENDCLASS.
