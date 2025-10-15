class ZCL_CLIENTES definition
  public
  final
  create public .

public section.

  interfaces ZIF_PARCEIROS .

  data AT_KNA1 type KNA1 .
protected section.
private section.
ENDCLASS.



CLASS ZCL_CLIENTES IMPLEMENTATION.


  METHOD zif_parceiros~ck_ativo.

    r_parceiro = me.

    SELECT SINGLE * INTO @DATA(wa_kna1)
      FROM kna1
     WHERE kunnr EQ @me->zif_parceiros~parceiro.

    IF sy-subrc IS NOT INITIAL.
      RAISE EXCEPTION TYPE zcx_parceiros
        EXPORTING
          textid = VALUE #( msgid = zcx_parceiros=>zcx_nao_cliente-msgid
                            msgno = zcx_parceiros=>zcx_nao_cliente-msgno
                            attr1 = CONV #( me->zif_parceiros~parceiro ) )
          msgid  = zcx_parceiros=>zcx_nao_cliente-msgid
          msgno  = zcx_parceiros=>zcx_nao_cliente-msgno
          msgty  = 'E'
          msgv1  = CONV #( me->zif_parceiros~parceiro ).
    ENDIF.

    IF wa_kna1-sperr EQ abap_true.
      RAISE EXCEPTION TYPE zcx_parceiros
        EXPORTING
          textid = VALUE #( msgid = zcx_parceiros=>zcx_cli_nao_contabiliza-msgid
                            msgno = zcx_parceiros=>zcx_cli_nao_contabiliza-msgno
                            attr1 = CONV #( me->zif_parceiros~parceiro ) )
          msgid  = zcx_parceiros=>zcx_cli_nao_contabiliza-msgid
          msgno  = zcx_parceiros=>zcx_cli_nao_contabiliza-msgno
          msgty  = 'E'
          msgv1  = CONV #( me->zif_parceiros~parceiro ).
    ENDIF.

    IF wa_kna1-loevm EQ abap_true.
      RAISE EXCEPTION TYPE zcx_parceiros
        EXPORTING
          textid = VALUE #( msgid = zcx_parceiros=>zcx_cli_marc_eliminacao-msgid
                            msgno = zcx_parceiros=>zcx_cli_marc_eliminacao-msgno
                            attr1 = CONV #( me->zif_parceiros~parceiro ) )
          msgid  = zcx_parceiros=>zcx_cli_marc_eliminacao-msgid
          msgno  = zcx_parceiros=>zcx_cli_marc_eliminacao-msgno
          msgty  = 'E'
          msgv1  = CONV #( me->zif_parceiros~parceiro ).
    ENDIF.

    IF wa_kna1-nodel EQ abap_true.
      RAISE EXCEPTION TYPE zcx_parceiros
        EXPORTING
          textid = VALUE #( msgid = zcx_parceiros=>zcx_cli_bloq_central-msgid
                            msgno = zcx_parceiros=>zcx_cli_bloq_central-msgno
                            attr1 = CONV #( me->zif_parceiros~parceiro ) )
          msgid  = zcx_parceiros=>zcx_cli_bloq_central-msgid
          msgno  = zcx_parceiros=>zcx_cli_bloq_central-msgno
          msgty  = 'E'
          msgv1  = CONV #( me->zif_parceiros~parceiro ).
    ENDIF.

    CHECK i_ck_sd IS NOT INITIAL.

    IF wa_kna1-aufsd EQ abap_true."  Bloqueio de ordem centralizado para cliente
      RAISE EXCEPTION TYPE zcx_parceiros
        EXPORTING
          textid = VALUE #( msgid = zcx_parceiros=>zcx_cli_bloq_ordem_center-msgid
                            msgno = zcx_parceiros=>zcx_cli_bloq_ordem_center-msgno
                            attr1 = CONV #( me->zif_parceiros~parceiro ) )
          msgid  = zcx_parceiros=>zcx_cli_bloq_ordem_center-msgid
          msgno  = zcx_parceiros=>zcx_cli_bloq_ordem_center-msgno
          msgty  = 'E'
          msgv1  = CONV #( me->zif_parceiros~parceiro ).
    ENDIF.

    IF wa_kna1-lifsd EQ abap_true."  Bloqueio de remessa centralizado para cliente
      RAISE EXCEPTION TYPE zcx_parceiros
        EXPORTING
          textid = VALUE #( msgid = zcx_parceiros=>zcx_cli_bloq_remessa_center-msgid
                            msgno = zcx_parceiros=>zcx_cli_bloq_remessa_center-msgno
                            attr1 = CONV #( me->zif_parceiros~parceiro ) )
          msgid  = zcx_parceiros=>zcx_cli_bloq_remessa_center-msgid
          msgno  = zcx_parceiros=>zcx_cli_bloq_remessa_center-msgno
          msgty  = 'E'
          msgv1  = CONV #( me->zif_parceiros~parceiro ).
    ENDIF.

    IF wa_kna1-faksd EQ abap_true. "Bloqueio centralizado de faturamento para cliente
      RAISE EXCEPTION TYPE zcx_parceiros
        EXPORTING
          textid = VALUE #( msgid = zcx_parceiros=>zcx_cli_bloq_fatura_center-msgid
                            msgno = zcx_parceiros=>zcx_cli_bloq_fatura_center-msgno
                            attr1 = CONV #( me->zif_parceiros~parceiro ) )
          msgid  = zcx_parceiros=>zcx_cli_bloq_fatura_center-msgid
          msgno  = zcx_parceiros=>zcx_cli_bloq_fatura_center-msgno
          msgty  = 'E'
          msgv1  = CONV #( me->zif_parceiros~parceiro ).
    ENDIF.

    IF wa_kna1-cassd EQ abap_true."  Bloqueio de contatos central para cliente
      RAISE EXCEPTION TYPE zcx_parceiros
        EXPORTING
          textid = VALUE #( msgid = zcx_parceiros=>zcx_cli_bloq_contato_central-msgid
                            msgno = zcx_parceiros=>zcx_cli_bloq_contato_central-msgno
                            attr1 = CONV #( me->zif_parceiros~parceiro ) )
          msgid  = zcx_parceiros=>zcx_cli_bloq_contato_central-msgid
          msgno  = zcx_parceiros=>zcx_cli_bloq_contato_central-msgno
          msgty  = 'E'
          msgv1  = CONV #( me->zif_parceiros~parceiro ).
    ENDIF.

  ENDMETHOD.


  METHOD ZIF_PARCEIROS~CK_ATIVO_EMPRESA.

    R_PARCEIRO = ME.

    CHECK I_EMPRESA IS NOT INITIAL.

    SELECT SINGLE * INTO @DATA(WA_KNB1)
      FROM KNB1
     WHERE KUNNR EQ @ME->ZIF_PARCEIROS~PARCEIRO
       AND BUKRS EQ @I_EMPRESA.

    IF SY-SUBRC IS NOT INITIAL.
      RAISE EXCEPTION TYPE ZCX_PARCEIROS
        EXPORTING
          TEXTID = VALUE #( MSGID = ZCX_PARCEIROS=>ZCX_CLI_NAO_EMPRESA-MSGID
                            MSGNO = ZCX_PARCEIROS=>ZCX_CLI_NAO_EMPRESA-MSGNO
                            ATTR1 = CONV #( ME->ZIF_PARCEIROS~PARCEIRO )
                            ATTR2 = CONV #( I_EMPRESA ) )
          MSGID  = ZCX_PARCEIROS=>ZCX_CLI_NAO_EMPRESA-MSGID
          MSGNO  = ZCX_PARCEIROS=>ZCX_CLI_NAO_EMPRESA-MSGNO
          MSGTY  = 'E'
          MSGV1  = CONV #( ME->ZIF_PARCEIROS~PARCEIRO )
          MSGV2  = CONV #( I_EMPRESA ).
    ENDIF.

    IF WA_KNB1-SPERR EQ ABAP_TRUE.
      RAISE EXCEPTION TYPE ZCX_PARCEIROS
        EXPORTING
          TEXTID = VALUE #( MSGID = ZCX_PARCEIROS=>ZCX_CLI_NAO_CONTABILIZA_EMP-MSGID
                            MSGNO = ZCX_PARCEIROS=>ZCX_CLI_NAO_CONTABILIZA_EMP-MSGNO
                            ATTR1 = CONV #( ME->ZIF_PARCEIROS~PARCEIRO )
                            ATTR2 = CONV #( I_EMPRESA ) )
          MSGID  = ZCX_PARCEIROS=>ZCX_CLI_NAO_CONTABILIZA_EMP-MSGID
          MSGNO  = ZCX_PARCEIROS=>ZCX_CLI_NAO_CONTABILIZA_EMP-MSGNO
          MSGTY  = 'E'
          MSGV1  = CONV #( ME->ZIF_PARCEIROS~PARCEIRO )
          MSGV2  = CONV #( I_EMPRESA ).
    ENDIF.

    IF WA_KNB1-LOEVM EQ ABAP_TRUE.
      RAISE EXCEPTION TYPE ZCX_PARCEIROS
        EXPORTING
          TEXTID = VALUE #( MSGID = ZCX_PARCEIROS=>ZCX_CLI_MARC_ELIMINACAO_EMP-MSGID
                            MSGNO = ZCX_PARCEIROS=>ZCX_CLI_MARC_ELIMINACAO_EMP-MSGNO
                            ATTR1 = CONV #( ME->ZIF_PARCEIROS~PARCEIRO )
                            ATTR2 = CONV #( I_EMPRESA ) )
          MSGID  = ZCX_PARCEIROS=>ZCX_CLI_MARC_ELIMINACAO_EMP-MSGID
          MSGNO  = ZCX_PARCEIROS=>ZCX_CLI_MARC_ELIMINACAO_EMP-MSGNO
          MSGTY  = 'E'
          MSGV1  = CONV #( ME->ZIF_PARCEIROS~PARCEIRO )
          MSGV2  = CONV #( I_EMPRESA ).
    ENDIF.

  ENDMETHOD.


  method ZIF_PARCEIROS~CK_EMISSOR_NF_E.

    R_PARCEIRO = ME.

    RAISE EXCEPTION TYPE ZCX_PARCEIROS
      EXPORTING
        TEXTID = VALUE #( MSGID = ZCX_PARCEIROS=>ZCX_NAO_EMISSOR_NFE-MSGID
                          MSGNO = ZCX_PARCEIROS=>ZCX_NAO_EMISSOR_NFE-MSGNO
                          ATTR1 = CONV #( ME->ZIF_PARCEIROS~PARCEIRO ) )
        MSGID  = ZCX_PARCEIROS=>ZCX_NAO_EMISSOR_NFE-MSGID
        MSGNO  = ZCX_PARCEIROS=>ZCX_NAO_EMISSOR_NFE-MSGNO
        MSGTY  = 'E'
        MSGV1  = CONV #( ME->ZIF_PARCEIROS~PARCEIRO ).

  endmethod.


  METHOD ZIF_PARCEIROS~CK_PARCEIRO_EMP_DIFERENTE.

    R_PARCEIRO = ME.

    DATA(_EMP_IGUAIS) = ABAP_FALSE.

    TRY .

      ME->ZIF_PARCEIROS~CK_PARCEIRO_INTERCOMPANY(
        EXPORTING
          I_EMPRESA     = I_EMPRESA    " Empresa
        IMPORTING
          E_J_1BBRANCH  = E_J_1BBRANCH    " Local de negócios
      ).

      _EMP_IGUAIS = ABAP_TRUE.


    CATCH ZCX_PARCEIROS.
    ENDTRY.

    IF _EMP_IGUAIS EQ ABAP_TRUE.

      RAISE EXCEPTION TYPE ZCX_PARCEIROS
        EXPORTING
          TEXTID = VALUE #( MSGID = ZCX_PARCEIROS=>ZCX_PARCEIRO_EMP_IGUAIS-MSGID
                            MSGNO = ZCX_PARCEIROS=>ZCX_PARCEIRO_EMP_IGUAIS-MSGNO
                            ATTR1 = CONV #( ME->ZIF_PARCEIROS~PARCEIRO )
                            ATTR2 = CONV #( I_EMPRESA ) )
          MSGID  = ZCX_PARCEIROS=>ZCX_PARCEIRO_EMP_IGUAIS-MSGID
          MSGNO  = ZCX_PARCEIROS=>ZCX_PARCEIRO_EMP_IGUAIS-MSGNO
          MSGTY  = 'E'
          MSGV1  = CONV #( ME->ZIF_PARCEIROS~PARCEIRO )
          MSGV2  = CONV #( I_EMPRESA ).

    ENDIF.



  ENDMETHOD.


  METHOD ZIF_PARCEIROS~CK_PARCEIRO_INTERCOMPANY.

    CLEAR: E_J_1BBRANCH.

    R_PARCEIRO = ME.

    DATA(R_LOCAL_NEGOCIO) =
       ZCL_PARCEIRO=>GET_PARCEIRO_LOCAL_NEGOCIO(
          EXPORTING
            I_PARTINER = ME->ZIF_PARCEIROS~PARCEIRO
          IMPORTING
            E_J_1BBRANCH = E_J_1BBRANCH
       ).

    IF R_LOCAL_NEGOCIO EQ ABAP_FALSE.
      RAISE EXCEPTION TYPE ZCX_PARCEIROS
        EXPORTING
          TEXTID = VALUE #( MSGID = ZCX_PARCEIROS=>ZCX_NAO_LOCAL_NEGOCIO-MSGID
                            MSGNO = ZCX_PARCEIROS=>ZCX_NAO_LOCAL_NEGOCIO-MSGNO
                            ATTR1 = CONV #( ME->ZIF_PARCEIROS~PARCEIRO ) )
          MSGID  = ZCX_PARCEIROS=>ZCX_NAO_LOCAL_NEGOCIO-MSGID
          MSGNO  = ZCX_PARCEIROS=>ZCX_NAO_LOCAL_NEGOCIO-MSGNO
          MSGTY  = 'E'
          MSGV1  = CONV #( ME->ZIF_PARCEIROS~PARCEIRO ).
    ENDIF.

    IF I_EMPRESA NE E_J_1BBRANCH-BUKRS.
      RAISE EXCEPTION TYPE ZCX_PARCEIROS
        EXPORTING
          TEXTID = VALUE #( MSGID = ZCX_PARCEIROS=>ZCX_PARCEIRO_NAO_INTERCOMPANY-MSGID
                            MSGNO = ZCX_PARCEIROS=>ZCX_PARCEIRO_NAO_INTERCOMPANY-MSGNO
                            ATTR1 = CONV #( ME->ZIF_PARCEIROS~PARCEIRO )
                            ATTR2 = CONV #( I_EMPRESA ) )
          MSGID  = ZCX_PARCEIROS=>ZCX_PARCEIRO_NAO_INTERCOMPANY-MSGID
          MSGNO  = ZCX_PARCEIROS=>ZCX_PARCEIRO_NAO_INTERCOMPANY-MSGNO
          MSGTY  = 'E'
          MSGV1  = CONV #( ME->ZIF_PARCEIROS~PARCEIRO )
          MSGV2  = CONV #( I_EMPRESA ).
    ENDIF.

  ENDMETHOD.


  METHOD ZIF_PARCEIROS~CK_PARCEIRO_LOCAL_NEGOCIO.

    R_PARCEIRO = ME.

    DATA(R_LOCAL_NEGOCIO) = ZCL_PARCEIRO=>GET_PARCEIRO_LOCAL_NEGOCIO( EXPORTING I_PARTINER = ME->ZIF_PARCEIROS~PARCEIRO
                                                                      IMPORTING E_J_1BBRANCH = E_J_1BBRANCH
                                                                       ).

    IF R_LOCAL_NEGOCIO EQ ABAP_FALSE.
      RAISE EXCEPTION TYPE ZCX_PARCEIROS
        EXPORTING
          TEXTID = VALUE #( MSGID = ZCX_PARCEIROS=>ZCX_NAO_LOCAL_NEGOCIO-MSGID
                            MSGNO = ZCX_PARCEIROS=>ZCX_NAO_LOCAL_NEGOCIO-MSGNO
                            ATTR1 = CONV #( ME->ZIF_PARCEIROS~PARCEIRO ) )
          MSGID  = ZCX_PARCEIROS=>ZCX_NAO_LOCAL_NEGOCIO-MSGID
          MSGNO  = ZCX_PARCEIROS=>ZCX_NAO_LOCAL_NEGOCIO-MSGNO
          MSGTY  = 'E'
          MSGV1  = CONV #( ME->ZIF_PARCEIROS~PARCEIRO ).
    ENDIF.

  ENDMETHOD.


  METHOD ZIF_PARCEIROS~CK_PARCEIRO_TERCEIRO.

    R_PARCEIRO = ME.

    DATA(R_LOCAL_NEGOCIO) = ZCL_PARCEIRO=>GET_PARCEIRO_LOCAL_NEGOCIO( I_PARTINER = ME->ZIF_PARCEIROS~PARCEIRO ).

    IF R_LOCAL_NEGOCIO EQ ABAP_TRUE.
      RAISE EXCEPTION TYPE ZCX_PARCEIROS
        EXPORTING
          TEXTID = VALUE #( MSGID = ZCX_PARCEIROS=>ZCX_NAO_TERCEIRO-MSGID
                            MSGNO = ZCX_PARCEIROS=>ZCX_NAO_TERCEIRO-MSGNO
                            ATTR1 = CONV #( ME->ZIF_PARCEIROS~PARCEIRO ) )
          MSGID  = ZCX_PARCEIROS=>ZCX_NAO_TERCEIRO-MSGID
          MSGNO  = ZCX_PARCEIROS=>ZCX_NAO_TERCEIRO-MSGNO
          MSGTY  = 'E'
          MSGV1  = CONV #( ME->ZIF_PARCEIROS~PARCEIRO ).
    ENDIF.

  ENDMETHOD.


  METHOD ZIF_PARCEIROS~CK_RESTRICAO_EMBARGO.

    "Cliente não tem restrição pois é utilizado para venda.
    R_PARCEIRO = ME.

  ENDMETHOD.


  METHOD ZIF_PARCEIROS~CK_SERVICO_FRETE.

    R_INSTANCE = ME.

    RAISE EXCEPTION TYPE ZCX_PARCEIROS
      EXPORTING
        TEXTID = VALUE #( MSGID = ZCX_PARCEIROS=>ZCX_CLIENTE_NAO_FORNECEDOR-MSGID
                          MSGNO = ZCX_PARCEIROS=>ZCX_CLIENTE_NAO_FORNECEDOR-MSGNO )
        MSGID  = ZCX_PARCEIROS=>ZCX_CLIENTE_NAO_FORNECEDOR-MSGID
        MSGNO  = ZCX_PARCEIROS=>ZCX_CLIENTE_NAO_FORNECEDOR-MSGNO
        MSGTY  = 'E'.

  ENDMETHOD.


  METHOD ZIF_PARCEIROS~CK_SERVICO_FRETE_RODO.

    R_INSTANCE = ME.

    RAISE EXCEPTION TYPE ZCX_PARCEIROS
      EXPORTING
        TEXTID = VALUE #( MSGID = ZCX_PARCEIROS=>ZCX_CLIENTE_NAO_FORNECEDOR-MSGID
                          MSGNO = ZCX_PARCEIROS=>ZCX_CLIENTE_NAO_FORNECEDOR-MSGNO )
        MSGID  = ZCX_PARCEIROS=>ZCX_CLIENTE_NAO_FORNECEDOR-MSGID
        MSGNO  = ZCX_PARCEIROS=>ZCX_CLIENTE_NAO_FORNECEDOR-MSGNO
        MSGTY  = 'E'.

  ENDMETHOD.


  method ZIF_PARCEIROS~GERA_ERRO_GERAL.

    DATA: LC_TEXTO TYPE C LENGTH 200.
    LC_TEXTO = I_TEXTO.
    SY-MSGV1 = LC_TEXTO+000(50).
    SY-MSGV2 = LC_TEXTO+050(50).
    SY-MSGV3 = LC_TEXTO+100(50).
    SY-MSGV4 = LC_TEXTO+150(50).

    RAISE EXCEPTION TYPE ZCX_PARCEIROS
      EXPORTING
        TEXTID = VALUE #( MSGID = ZCX_PARCEIROS=>ZCX_ERRO_GERAL-MSGID
                          MSGNO = ZCX_PARCEIROS=>ZCX_ERRO_GERAL-MSGNO
                          ATTR1 = CONV #( SY-MSGV1 )
                          ATTR2 = CONV #( SY-MSGV2 )
                          ATTR3 = CONV #( SY-MSGV3 )
                          ATTR4 = CONV #( SY-MSGV4 ) )
        MSGID  = ZCX_PARCEIROS=>ZCX_ERRO_GERAL-MSGID
        MSGNO  = ZCX_PARCEIROS=>ZCX_ERRO_GERAL-MSGNO
        MSGTY  = 'E'
        MSGV1  = SY-MSGV1
        MSGV2  = SY-MSGV2
        MSGV3  = SY-MSGV3
        MSGV4  = SY-MSGV4.

  endmethod.


  METHOD ZIF_PARCEIROS~GET_DADOS_BANCARIOS.

    R_PARCEIRO = ME.

    RAISE EXCEPTION TYPE ZCX_PARCEIROS
      EXPORTING
        TEXTID = VALUE #( MSGID = ZCX_PARCEIROS=>ZCX_COORDENADAS_BANCARIAS-MSGID
                          MSGNO = ZCX_PARCEIROS=>ZCX_COORDENADAS_BANCARIAS-MSGNO
                          ATTR1 = CONV #( ME->ZIF_PARCEIROS~PARCEIRO ) )
        MSGID  = ZCX_PARCEIROS=>ZCX_COORDENADAS_BANCARIAS-MSGID
        MSGNO  = ZCX_PARCEIROS=>ZCX_COORDENADAS_BANCARIAS-MSGNO
        MSGTY  = 'E'
        MSGV1  = CONV #( ME->ZIF_PARCEIROS~PARCEIRO ).

  ENDMETHOD.


  METHOD ZIF_PARCEIROS~GET_EMAIL.

    R_PARCEIRO = ME.

    DATA(LC_CHAVE) = SY-MANDT && ME->ZIF_PARCEIROS~PARCEIRO.

    SELECT ADDRNUMBER INTO @DATA(LC_ADDRNUMBER)
      FROM ADRV UP TO 1 ROWS
     WHERE APPL_TABLE EQ 'KNA1'
       AND APPL_FIELD EQ 'ADRNR'
       AND APPL_KEY   EQ @LC_CHAVE   "Mandt && Id Cliente
       AND OWNER      EQ @ABAP_TRUE
     ORDER BY APPL_TABLE.
    ENDSELECT.

    CHECK LC_ADDRNUMBER IS NOT INITIAL.

    SELECT SMTP_ADDR INTO E_MAIL UP TO 1 ROWS
      FROM ADR6
     WHERE ADDRNUMBER EQ LC_ADDRNUMBER.
    ENDSELECT.

  ENDMETHOD.


  METHOD ZIF_PARCEIROS~GET_ENDERECO.

    DATA: LC_RETORNO   TYPE AD_RETCODE,
          IT_ADDVALUE  TYPE TABLE OF ADDR1_VAL,
          IT_ADDSELECT TYPE TABLE OF ADDR1_SEL.

    R_PARCEIRO = ME.

    DATA(LC_CHAVE) = SY-MANDT && ME->ZIF_PARCEIROS~PARCEIRO.

    SELECT ADDRNUMBER FROM ADRV UP TO 1 ROWS
              INTO @DATA(LC_ADDRNUMBER)
             WHERE APPL_TABLE EQ 'KNA1'
               AND APPL_FIELD EQ 'ADRNR'
               AND APPL_KEY   EQ @LC_CHAVE   "Mandt && Id Fornecedor
               AND OWNER      EQ @ABAP_TRUE
             ORDER BY APPL_TABLE.
      APPEND VALUE #( ADDRNUMBER = LC_ADDRNUMBER ) TO IT_ADDSELECT.
    ENDSELECT.

    CHECK IT_ADDSELECT[] IS NOT INITIAL.

    CALL FUNCTION 'ADDR_GET_ARRAY'
      IMPORTING
        RETURNCODE        = LC_RETORNO
      TABLES
        ADDRESS_SELECTION = IT_ADDSELECT
        ADDRESS_VALUE     = IT_ADDVALUE
      EXCEPTIONS
        PARAMETER_ERROR   = 1
        INTERNAL_ERROR    = 2
        OTHERS            = 3.

    IF SY-SUBRC IS NOT INITIAL.
      MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO INTO DATA(MTEXT) WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
      ZCX_PARCEIROS=>ZIF_ERROR~GERA_ERRO_GERAL( I_TEXTO = MTEXT ).
    ENDIF.

    READ TABLE IT_ADDVALUE INTO E_ENDERECO INDEX 1.

  ENDMETHOD.


  METHOD ZIF_PARCEIROS~GET_ID_PARCEIRO.

    R_PARCEIRO = ME.
    E_PARCEIRO = ME->ZIF_PARCEIROS~PARCEIRO.

  ENDMETHOD.


  METHOD ZIF_PARCEIROS~GET_INSTANCE.

    IF ZIF_PARCEIROS~AT_PARCEIROS IS NOT BOUND.
      CREATE OBJECT ZIF_PARCEIROS~AT_PARCEIROS TYPE ZCL_CLIENTES.
      PARCEIRO = ZIF_PARCEIROS~AT_PARCEIROS.
    ELSE.
      PARCEIRO = ZIF_PARCEIROS~AT_PARCEIROS.
    ENDIF.

  ENDMETHOD.


  METHOD ZIF_PARCEIROS~GET_NAME.

    R_PARCEIRO = ME.
    E_NAME = ME->ZIF_PARCEIROS~NOME.

  ENDMETHOD.


  method ZIF_PARCEIROS~GET_REGIO.

    R_PARCEIRO = ME.
    E_REGIO = ME->ZIF_PARCEIROS~REGIO.
    E_LAND1 = ME->ZIF_PARCEIROS~LAND1.

  endmethod.


  METHOD ZIF_PARCEIROS~GET_TIPO_PARCEIRO.

    R_PARCEIRO = ME.

    CASE ME->ZIF_PARCEIROS~TIPO_PESSOA.
      WHEN ABAP_FALSE.
        E_TIPO = ZIF_PARCEIROS=>ST_PESSOA_JURIDICA.
      WHEN ABAP_TRUE.
        E_TIPO = ZIF_PARCEIROS=>ST_PESSOA_FISICA.
    ENDCASE.

  ENDMETHOD.


  METHOD ZIF_PARCEIROS~GET_TXJCD.

    R_PARCEIRO = ME.
    E_TXJCD = ME->ZIF_PARCEIROS~TXJCD.

  ENDMETHOD.


  METHOD ZIF_PARCEIROS~SET_PARCEIRO.
    R_PARCEIRO = ME.

    SELECT SINGLE * INTO @DATA(WA_KNA1)
      FROM KNA1
     WHERE KUNNR EQ @I_PARCEIRO.

    IF SY-SUBRC IS NOT INITIAL.
      RAISE EXCEPTION TYPE ZCX_PARCEIROS
        EXPORTING
          TEXTID = VALUE #( MSGID = ZCX_PARCEIROS=>ZCX_NAO_CLIENTE-MSGID
                            MSGNO = ZCX_PARCEIROS=>ZCX_NAO_CLIENTE-MSGNO
                            ATTR1 = CONV #( I_PARCEIRO ) )
          MSGID  = ZCX_PARCEIROS=>ZCX_NAO_CLIENTE-MSGID
          MSGNO  = ZCX_PARCEIROS=>ZCX_NAO_CLIENTE-MSGNO
          MSGTY  = 'E'
          MSGV1  = CONV #( I_PARCEIRO ).
    ENDIF.

    ME->ZIF_PARCEIROS~PARCEIRO    = I_PARCEIRO.
    ME->ZIF_PARCEIROS~NOME        = WA_KNA1-NAME1.
    ME->ZIF_PARCEIROS~TIPO_PESSOA = WA_KNA1-STKZN.
    ME->ZIF_PARCEIROS~REGIO       = WA_KNA1-REGIO.
    ME->ZIF_PARCEIROS~LAND1       = WA_KNA1-LAND1.
    ME->ZIF_PARCEIROS~TXJCD       = WA_KNA1-TXJCD.
    ME->AT_KNA1                   = WA_KNA1.

  ENDMETHOD.


  METHOD zif_parceiros~set_parceiro_cnpj_cpf_ie.

    DATA: rg_cnpj TYPE RANGE OF stcd1,
          rg_cpf  TYPE RANGE OF stcd2,
          rg_ie   TYPE RANGE OF stcd3.

    DATA: wa_kna1 TYPE kna1.

    r_parceiro = me.

    wa_kna1-stcd1 = i_cnpj.
    wa_kna1-stcd2 = i_cpf.
    wa_kna1-stcd3 = i_insc_estatual.

    IF wa_kna1-stcd3 IS NOT INITIAL.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
        EXPORTING
          input  = wa_kna1-stcd3
        IMPORTING
          output = wa_kna1-stcd3.
    ENDIF.

    IF i_cnpj IS INITIAL AND i_cpf IS INITIAL.
      RAISE EXCEPTION TYPE zcx_parceiros
        EXPORTING
          textid = VALUE #( msgid = zcx_parceiros=>zcx_informar_cnpj_cpf-msgid
                            msgno = zcx_parceiros=>zcx_informar_cnpj_cpf-msgno )
          msgid  = zcx_parceiros=>zcx_informar_cnpj_cpf-msgid
          msgno  = zcx_parceiros=>zcx_informar_cnpj_cpf-msgno
          msgty  = 'E'.
    ENDIF.

    IF i_cnpj IS NOT INITIAL.
      rg_cnpj = VALUE #( sign = 'I' option = 'EQ' ( high = wa_kna1-stcd1 low = wa_kna1-stcd1 ) ).
    ENDIF.

    IF i_cpf IS NOT INITIAL.
      rg_cpf  = VALUE #( sign = 'I' option = 'EQ' ( high = wa_kna1-stcd2 low = wa_kna1-stcd2 ) ).
    ENDIF.

    IF i_insc_estatual IS NOT INITIAL.
      rg_ie = VALUE #( sign = 'I' option = 'CP' ( low = '*' && wa_kna1-stcd3 && '*' ) ).
    ENDIF.

    SELECT SINGLE * INTO wa_kna1
      FROM kna1
     WHERE stcd1 IN rg_cnpj
       AND stcd2 IN rg_cpf
       AND stcd3 IN rg_ie
       AND loevm NE abap_true.

    IF sy-subrc IS NOT INITIAL.
      RAISE EXCEPTION TYPE zcx_parceiros
        EXPORTING
          textid = VALUE #( msgid = zcx_parceiros=>zcx_nao_fornecedor-msgid
                            msgno = zcx_parceiros=>zcx_nao_fornecedor-msgno
                            attr1 = CONV #( me->zif_parceiros~parceiro ) )
          msgid  = zcx_parceiros=>zcx_nao_fornecedor-msgid
          msgno  = zcx_parceiros=>zcx_nao_fornecedor-msgno
          msgty  = 'E'
          msgv1  = CONV #( me->zif_parceiros~parceiro ).
    ENDIF.

    me->zif_parceiros~set_parceiro_tabela( EXPORTING i_tabela = wa_kna1 ).

  ENDMETHOD.


  METHOD ZIF_PARCEIROS~SET_PARCEIRO_IE.

    DATA: LC_STCD3 TYPE KNA1-STCD3.

    DATA: LC_STCD3_AUX1 TYPE KNA1-STCD3,
          LC_STCD3_AUX2 TYPE KNA1-STCD3.

    R_PARCEIRO = ME.

    CHECK I_INSC_ESTATUAL IS NOT INITIAL.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        INPUT  = I_INSC_ESTATUAL
      IMPORTING
        OUTPUT = LC_STCD3_AUX1.

    LC_STCD3 = '%' && LC_STCD3_AUX1.

    SELECT *
      FROM KNA1 INTO TABLE @DATA(TG_KNA1)
     WHERE STCD3 LIKE @LC_STCD3.

    LOOP AT TG_KNA1 INTO DATA(WA_KNA1).
      DATA(_TABIX) = SY-TABIX.
      DATA(_DEL)   = ''.

      CLEAR: LC_STCD3_AUX2.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
        EXPORTING
          INPUT  = WA_KNA1-STCD3
        IMPORTING
          OUTPUT = LC_STCD3_AUX2.

      IF LC_STCD3_AUX1 NE LC_STCD3_AUX2.
        _DEL = 'X'.
      ENDIF.

      TRY .
          ME->ZIF_PARCEIROS~SET_PARCEIRO_TABELA( EXPORTING I_TABELA = WA_KNA1
            )->CK_ATIVO( ).
        CATCH ZCX_PARCEIROS.
          _DEL = 'X'.
      ENDTRY.

      IF _DEL IS NOT INITIAL.
        DELETE TG_KNA1 INDEX _TABIX.
      ENDIF.
    ENDLOOP.

    READ TABLE TG_KNA1 INTO WA_KNA1 INDEX 1.

    IF ( SY-SUBRC NE 0 ) OR ( TG_KNA1[] IS INITIAL ).
      RAISE EXCEPTION TYPE ZCX_PARCEIROS
        EXPORTING
          TEXTID = VALUE #( MSGID = ZCX_PARCEIROS=>ZCX_NAO_CLIENTE-MSGID
                            MSGNO = ZCX_PARCEIROS=>ZCX_NAO_CLIENTE-MSGNO
                            ATTR1 = CONV #( I_INSC_ESTATUAL ) )
          MSGID  = ZCX_PARCEIROS=>ZCX_NAO_CLIENTE-MSGID
          MSGNO  = ZCX_PARCEIROS=>ZCX_NAO_CLIENTE-MSGNO
          MSGTY  = 'E'
          MSGV1  = CONV #( I_INSC_ESTATUAL ).
    ENDIF.

    ME->ZIF_PARCEIROS~SET_PARCEIRO_TABELA( EXPORTING I_TABELA = WA_KNA1 ).

  ENDMETHOD.


  METHOD ZIF_PARCEIROS~SET_PARCEIRO_TABELA.

    DATA: WA_KNA1 TYPE KNA1.

    R_INSTANCE = ME.

    MOVE-CORRESPONDING I_TABELA TO WA_KNA1.

    ME->ZIF_PARCEIROS~PARCEIRO      = WA_KNA1-KUNNR.
    ME->ZIF_PARCEIROS~NOME          = WA_KNA1-NAME1.
    ME->ZIF_PARCEIROS~TIPO_PESSOA   = WA_KNA1-STKZN.
    ME->ZIF_PARCEIROS~INSC_ESTADUAL = WA_KNA1-STCD3.
    ME->ZIF_PARCEIROS~REGIO         = WA_KNA1-REGIO.

    ME->AT_KNA1 = WA_KNA1.

  ENDMETHOD.
ENDCLASS.
