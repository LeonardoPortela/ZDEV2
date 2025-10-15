class ZCL_ORDEM_CARREGAMENTO definition
  public
  final
  create public .

public section.

  interfaces ZIF_PESQUISA .

  constants ST_TP_STATUS_ABERTO type ZDE_TP_STATUS_ORDEM value 'AB' ##NO_TEXT.
  constants ST_TP_STATUS_FECHADO type ZDE_TP_STATUS_ORDEM value 'FE' ##NO_TEXT.
  constants ST_TP_STATUS_CANCELADO type ZDE_TP_STATUS_ORDEM value 'CA' ##NO_TEXT.
  class-data AT_ORDEM type ref to ZCL_ORDEM_CARREGAMENTO .
  data ORDEM type ZSDT0001OD .

  class-methods GET_ORDEM_CARREGAMENTO
    importing
      !I_FILTRO type ZDE_FILTRO_ZSDT0001OD
    returning
      value(R_ORDEM_CARRGAMENTO) type ZDE_ZSDT0001OD_ALV
    raising
      ZCX_ORDEM_CARREGAMENTO .
  class-methods BUSCA_ORDEM_CARREGAMENTO
    importing
      !I_ID_ORDEM type ZDE_ID_ORDEM
    returning
      value(R_ORDEM_CARRGAMENTO) type ZDE_ZSDT0001OD_ALV
    raising
      ZCX_ORDEM_CARREGAMENTO .
  class-methods BUSCA_ORDEM_CARREGAMENTO_NR
    importing
      !I_NR_SAFRA type ZDE_NR_SAFRA
      !I_ID_BUKRS type ZDE_BUKRS_RECEB
      !I_ID_BRANCH type ZDE_BRANCH_RECEB
      !I_NR_ORDEM type ZDE_NR_ORDEM
    returning
      value(R_ORDEM_CARRGAMENTO) type ZDE_ZSDT0001OD_ALV
    raising
      ZCX_ORDEM_CARREGAMENTO .
  class-methods SET_ABRIR
    importing
      !I_ID_ORDEM type ZDE_ID_ORDEM
    returning
      value(R_ABRIU) type CHAR01
    raising
      ZCX_ORDEM_CARREGAMENTO .
  class-methods SET_FECHAR
    importing
      !I_ID_ORDEM type ZDE_ID_ORDEM
    returning
      value(R_FECHOU) type CHAR01
    raising
      ZCX_ORDEM_CARREGAMENTO .
  class-methods GET_INSTANCE
    returning
      value(R_INSTANCE) type ref to ZCL_ORDEM_CARREGAMENTO .
  methods SET_ORDEM
    importing
      !I_ID_ORDEM type ZDE_ID_ORDEM
    returning
      value(R_INSTANCE) type ref to ZCL_ORDEM_CARREGAMENTO
    raising
      ZCX_ORDEM_CARREGAMENTO .
  methods GET_CK_VIAGEM_CARGUERO
    exporting
      !E_VIAGEM_ID type ZDE_VIAGEM_ID
    returning
      value(R_INSTANCE) type ref to ZCL_ORDEM_CARREGAMENTO
    raising
      ZCX_ORDEM_CARREGAMENTO .
  methods GET_CK_VIAGEM_AUTORIZADA
    returning
      value(R_INSTANCE) type ref to ZCL_ORDEM_CARREGAMENTO
    raising
      ZCX_ORDEM_CARREGAMENTO .
  methods GET_CK_VIAGEM_NAO_CARREGADA
    returning
      value(R_INSTANCE) type ref to ZCL_ORDEM_CARREGAMENTO
    raising
      ZCX_ORDEM_CARREGAMENTO .
  methods SET_AUTORIZAR_PRECO_CARGUERO
    importing
      !I_PRECO type ZVALOR_FRETE
    returning
      value(R_INSTANCE) type ref to ZCL_ORDEM_CARREGAMENTO
    raising
      ZCX_ORDEM_CARREGAMENTO .
protected section.
private section.
ENDCLASS.



CLASS ZCL_ORDEM_CARREGAMENTO IMPLEMENTATION.


  METHOD BUSCA_ORDEM_CARREGAMENTO.

    DATA: I_FILTRO    TYPE ZDE_FILTRO_ZSDT0001OD,
          WA_ID_ORDEM TYPE ZDE_ID_ORDEM_R.

    WA_ID_ORDEM-SIGN   = 'I'.
    WA_ID_ORDEM-OPTION = 'EQ'.
    WA_ID_ORDEM-LOW    = I_ID_ORDEM.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        INPUT  = WA_ID_ORDEM-LOW
      IMPORTING
        OUTPUT = WA_ID_ORDEM-LOW.

    WA_ID_ORDEM-HIGH   = WA_ID_ORDEM-LOW.

    APPEND  WA_ID_ORDEM TO I_FILTRO-IIDORDEM.

    R_ORDEM_CARRGAMENTO = ZCL_ORDEM_CARREGAMENTO=>GET_ORDEM_CARREGAMENTO( I_FILTRO = I_FILTRO ).

  ENDMETHOD.


  METHOD BUSCA_ORDEM_CARREGAMENTO_NR.

    DATA: I_FILTRO     TYPE ZDE_FILTRO_ZSDT0001OD,
          WA_NR_SAFRA  TYPE ZDE_NR_SAFRA_R,
          WA_NR_BUKRS  TYPE ZDE_BUKRS_RECEB_R,
          WA_NR_BRANCH TYPE ZDE_BRANCH_RECEB_R,
          WA_NR_ORDEM  TYPE ZDE_NR_ORDEM_R.

    CLEAR: R_ORDEM_CARRGAMENTO.

    WA_NR_SAFRA-SIGN   = 'I'.
    WA_NR_SAFRA-OPTION = 'EQ'.
    WA_NR_SAFRA-LOW    = I_NR_SAFRA.
    WA_NR_SAFRA-HIGH   = WA_NR_SAFRA-LOW.
    APPEND WA_NR_SAFRA TO I_FILTRO-INRSAFRA.

    WA_NR_BUKRS-SIGN   = 'I'.
    WA_NR_BUKRS-OPTION = 'EQ'.
    WA_NR_BUKRS-LOW    = I_ID_BUKRS.
    WA_NR_BUKRS-HIGH   = WA_NR_BUKRS-LOW.
    APPEND WA_NR_BUKRS TO I_FILTRO-IIDBUKRS.

    WA_NR_BRANCH-SIGN   = 'I'.
    WA_NR_BRANCH-OPTION = 'EQ'.
    WA_NR_BRANCH-LOW    = I_ID_BRANCH.
    WA_NR_BRANCH-HIGH   = WA_NR_BRANCH-LOW.
    APPEND WA_NR_BRANCH TO I_FILTRO-IIDBRANC.

    WA_NR_ORDEM-SIGN   = 'I'.
    WA_NR_ORDEM-OPTION = 'EQ'.
    WA_NR_ORDEM-LOW    = I_NR_ORDEM.
    WA_NR_ORDEM-HIGH   = WA_NR_ORDEM-LOW.
    APPEND WA_NR_ORDEM TO I_FILTRO-INRORDEM.

    R_ORDEM_CARRGAMENTO = ZCL_ORDEM_CARREGAMENTO=>GET_ORDEM_CARREGAMENTO( I_FILTRO = I_FILTRO ).

  ENDMETHOD.


  METHOD GET_CK_VIAGEM_AUTORIZADA.

    R_INSTANCE = ME.

    SELECT SINGLE CK_AUTORIZADA INTO @DATA(LC_CK_AUTORIZADA) FROM ZLEST0185 WHERE ID_ORDEM EQ @ORDEM-ID_ORDEM.

    CHECK LC_CK_AUTORIZADA EQ ABAP_FALSE AND SY-SUBRC IS INITIAL.

    RAISE EXCEPTION TYPE ZCX_ORDEM_CARREGAMENTO
      EXPORTING
        TEXTID = VALUE #( MSGID = ZCX_ORDEM_CARREGAMENTO=>ZCX_VIAGEM_NAO_AUTORIZADA-MSGID
                          MSGNO = ZCX_ORDEM_CARREGAMENTO=>ZCX_VIAGEM_NAO_AUTORIZADA-MSGNO
                          ATTR1 = CONV #( ORDEM-NR_ORDEM ) )
        MSGTY  = 'E'
        MSGID  = ZCX_ORDEM_CARREGAMENTO=>ZCX_VIAGEM_NAO_AUTORIZADA-MSGID
        MSGNO  = ZCX_ORDEM_CARREGAMENTO=>ZCX_VIAGEM_NAO_AUTORIZADA-MSGNO
        MSGV1  = CONV #( ORDEM-NR_ORDEM ).


  ENDMETHOD.


  METHOD GET_CK_VIAGEM_CARGUERO.

    R_INSTANCE = ME.

    CLEAR: E_VIAGEM_ID.

    SELECT SINGLE VIAGEM_ID INTO @E_VIAGEM_ID FROM ZLEST0185 WHERE ID_ORDEM EQ @ORDEM-ID_ORDEM.

    CHECK SY-SUBRC IS NOT INITIAL.

    RAISE EXCEPTION TYPE ZCX_ORDEM_CARREGAMENTO
      EXPORTING
        TEXTID = VALUE #( MSGID = ZCX_ORDEM_CARREGAMENTO=>ZCX_SEM_VIAGEM_CARGUERO-MSGID
                          MSGNO = ZCX_ORDEM_CARREGAMENTO=>ZCX_SEM_VIAGEM_CARGUERO-MSGNO
                          ATTR1 = CONV #( ORDEM-ID_ORDEM ) )
        MSGTY  = 'E'
        MSGID  = ZCX_ORDEM_CARREGAMENTO=>ZCX_SEM_VIAGEM_CARGUERO-MSGID
        MSGNO  = ZCX_ORDEM_CARREGAMENTO=>ZCX_SEM_VIAGEM_CARGUERO-MSGNO
        MSGV1  = CONV #( ORDEM-ID_ORDEM ).

  ENDMETHOD.


  METHOD GET_CK_VIAGEM_NAO_CARREGADA.

    R_INSTANCE = ME.

    SELECT SINGLE CK_CARREGADO INTO @DATA(LC_CK_CARREGADO) FROM ZLEST0185 WHERE ID_ORDEM EQ @ORDEM-ID_ORDEM.

    CHECK LC_CK_CARREGADO EQ ABAP_TRUE AND SY-SUBRC IS INITIAL.

    RAISE EXCEPTION TYPE ZCX_ORDEM_CARREGAMENTO
      EXPORTING
        TEXTID = VALUE #( MSGID = ZCX_ORDEM_CARREGAMENTO=>ZCX_VIAGEM_CARREGADA-MSGID
                          MSGNO = ZCX_ORDEM_CARREGAMENTO=>ZCX_VIAGEM_CARREGADA-MSGNO
                          ATTR1 = CONV #( ORDEM-NR_ORDEM ) )
        MSGTY  = 'E'
        MSGID  = ZCX_ORDEM_CARREGAMENTO=>ZCX_VIAGEM_CARREGADA-MSGID
        MSGNO  = ZCX_ORDEM_CARREGAMENTO=>ZCX_VIAGEM_CARREGADA-MSGNO
        MSGV1  = CONV #( ORDEM-NR_ORDEM ).


  ENDMETHOD.


  METHOD GET_INSTANCE.

    IF AT_ORDEM IS NOT BOUND.
      CREATE OBJECT AT_ORDEM.
    ENDIF.
    R_INSTANCE = AT_ORDEM.

  ENDMETHOD.


  METHOD GET_ORDEM_CARREGAMENTO.

    DATA: LC_ORDEM   TYPE REF TO ZCL_ORDEM_CARREGAMENTO,
          IT_RETORNO TYPE ZDE_ZSDT0001OD_ALV_T.

    CLEAR: R_ORDEM_CARRGAMENTO.

    CREATE OBJECT LC_ORDEM.

    CALL METHOD LC_ORDEM->ZIF_PESQUISA~PESQUISAR
      EXPORTING
        I_FILTROS   = I_FILTRO
      IMPORTING
        E_REGISTROS = IT_RETORNO
      RECEIVING
        E_PESQUISOU = DATA(E_PESQUISOU).

    CLEAR LC_ORDEM.

    IF E_PESQUISOU IS INITIAL.
      RAISE EXCEPTION TYPE ZCX_ORDEM_CARREGAMENTO
        EXPORTING
          TEXTID = VALUE #( MSGID = ZCX_ORDEM_CARREGAMENTO=>ZCX_NAO_ENCONTRADO-MSGID
                            MSGNO = ZCX_ORDEM_CARREGAMENTO=>ZCX_NAO_ENCONTRADO-MSGNO )
          MSGTY  = 'E'
          MSGID  = ZCX_ORDEM_CARREGAMENTO=>ZCX_NAO_ENCONTRADO-MSGID
          MSGNO  = ZCX_ORDEM_CARREGAMENTO=>ZCX_NAO_ENCONTRADO-MSGNO.
    ENDIF.

    READ TABLE IT_RETORNO INDEX 1 INTO R_ORDEM_CARRGAMENTO.

  ENDMETHOD.


  METHOD SET_ABRIR.

    CLEAR R_ABRIU.

    DATA(R_ORDEM_CARRGAMENTO) = ZCL_ORDEM_CARREGAMENTO=>BUSCA_ORDEM_CARREGAMENTO( I_ID_ORDEM = I_ID_ORDEM ).

    IF R_ORDEM_CARRGAMENTO-TP_STATUS NE ST_TP_STATUS_FECHADO.
      RAISE EXCEPTION TYPE ZCX_ORDEM_CARREGAMENTO
        EXPORTING
          TEXTID = VALUE #( MSGID = ZCX_ORDEM_CARREGAMENTO=>ZCX_NAO_FECHADA-MSGID
                            MSGNO = ZCX_ORDEM_CARREGAMENTO=>ZCX_NAO_FECHADA-MSGNO
                            ATTR1 = CONV #( R_ORDEM_CARRGAMENTO-NR_ORDEM ) )
          MSGTY  = 'E'
          MSGID  = ZCX_ORDEM_CARREGAMENTO=>ZCX_NAO_FECHADA-MSGID
          MSGNO  = ZCX_ORDEM_CARREGAMENTO=>ZCX_NAO_FECHADA-MSGNO
          MSGV1  = CONV #( R_ORDEM_CARRGAMENTO-NR_ORDEM ).
    ENDIF.

    SELECT SINGLE * INTO @DATA(WA_ORDEM_CARRGAMENTO)
      FROM ZSDT0001OD
     WHERE ID_ORDEM EQ @R_ORDEM_CARRGAMENTO-ID_ORDEM.

    WA_ORDEM_CARRGAMENTO-TP_STATUS = ZCL_ORDEM_CARREGAMENTO=>ST_TP_STATUS_ABERTO.
    MODIFY ZSDT0001OD FROM WA_ORDEM_CARRGAMENTO.
    R_ABRIU = ABAP_TRUE.

  ENDMETHOD.


  METHOD SET_AUTORIZAR_PRECO_CARGUERO.

    R_INSTANCE = ME.

    TRY .
        ME->GET_CK_VIAGEM_CARGUERO( IMPORTING E_VIAGEM_ID = DATA(E_VIAGEM_ID) ).
      CATCH ZCX_ORDEM_CARREGAMENTO.
        EXIT.
    ENDTRY.

    ME->GET_CK_VIAGEM_AUTORIZADA(
     )->GET_CK_VIAGEM_NAO_CARREGADA(
     ).

    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Integração com o carguero de forma sincrona
    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    TRY .
        ZCL_INTEGRACAO_VIAGEM_PRECO=>ZIF_INTEGRACAO_VIAGEM_PRECO~GET_INSTANCE(
          )->SET_VIAGEM_PRECO_MOTORISTA(
            EXPORTING
              I_VIAGEM_ID = E_VIAGEM_ID
              I_WAERS     = 'BRL'
              I_FRETE     = I_PRECO
          ).
      CATCH ZCX_INTEGRACAO INTO DATA(EX_INTEGRA).    "

        RAISE EXCEPTION TYPE ZCX_ORDEM_CARREGAMENTO
          EXPORTING
            TEXTID = VALUE #( MSGID = EX_INTEGRA->MSGID MSGNO = EX_INTEGRA->MSGNO
                              ATTR1 = EX_INTEGRA->MSGV1 ATTR2 = EX_INTEGRA->MSGV2 ATTR3 = EX_INTEGRA->MSGV3 ATTR4 = EX_INTEGRA->MSGV4 )
            MSGTY  = 'E'
            MSGID  = EX_INTEGRA->MSGID
            MSGNO  = EX_INTEGRA->MSGNO
            MSGV1  = EX_INTEGRA->MSGV1
            MSGV2  = EX_INTEGRA->MSGV2
            MSGV3  = EX_INTEGRA->MSGV3
            MSGV4  = EX_INTEGRA->MSGV4.

      CATCH ZCX_ERROR INTO DATA(EX_ERRROR).    "

        RAISE EXCEPTION TYPE ZCX_ORDEM_CARREGAMENTO
          EXPORTING
            TEXTID = VALUE #( MSGID = EX_ERRROR->MSGID MSGNO = EX_ERRROR->MSGNO
                              ATTR1 = EX_ERRROR->MSGV1 ATTR2 = EX_ERRROR->MSGV2 ATTR3 = EX_ERRROR->MSGV3 ATTR4 = EX_ERRROR->MSGV4 )
            MSGTY  = 'E'
            MSGID  = EX_ERRROR->MSGID
            MSGNO  = EX_ERRROR->MSGNO
            MSGV1  = EX_ERRROR->MSGV1
            MSGV2  = EX_ERRROR->MSGV2
            MSGV3  = EX_ERRROR->MSGV3
            MSGV4  = EX_ERRROR->MSGV4.

    ENDTRY.


  ENDMETHOD.


  METHOD SET_FECHAR.

    CLEAR R_FECHOU.

    DATA(R_ORDEM_CARRGAMENTO) = ZCL_ORDEM_CARREGAMENTO=>BUSCA_ORDEM_CARREGAMENTO( I_ID_ORDEM = I_ID_ORDEM ).

    IF R_ORDEM_CARRGAMENTO-TP_STATUS NE ST_TP_STATUS_ABERTO.
      RAISE EXCEPTION TYPE ZCX_ORDEM_CARREGAMENTO
        EXPORTING
          TEXTID = VALUE #( MSGID = ZCX_ORDEM_CARREGAMENTO=>ZCX_NAO_ABERTA-MSGID
                            MSGNO = ZCX_ORDEM_CARREGAMENTO=>ZCX_NAO_ABERTA-MSGNO
                            ATTR1 = CONV #( R_ORDEM_CARRGAMENTO-NR_ORDEM ) )
          MSGTY  = 'E'
          MSGID  = ZCX_ORDEM_CARREGAMENTO=>ZCX_NAO_ABERTA-MSGID
          MSGNO  = ZCX_ORDEM_CARREGAMENTO=>ZCX_NAO_ABERTA-MSGNO
          MSGV1  = CONV #( R_ORDEM_CARRGAMENTO-NR_ORDEM ).
    ENDIF.

    SELECT SINGLE * INTO @DATA(WA_ORDEM_CARRGAMENTO)
      FROM ZSDT0001OD
     WHERE ID_ORDEM EQ @R_ORDEM_CARRGAMENTO-ID_ORDEM.

    WA_ORDEM_CARRGAMENTO-TP_STATUS = ZCL_ORDEM_CARREGAMENTO=>ST_TP_STATUS_FECHADO.
    MODIFY ZSDT0001OD FROM WA_ORDEM_CARRGAMENTO.
    R_FECHOU = ABAP_TRUE.

  ENDMETHOD.


  METHOD SET_ORDEM.

    R_INSTANCE = ME.

    SELECT SINGLE * INTO ME->ORDEM
      FROM ZSDT0001OD
     WHERE ID_ORDEM EQ I_ID_ORDEM.

    CHECK SY-SUBRC IS NOT INITIAL.

    RAISE EXCEPTION TYPE ZCX_ORDEM_CARREGAMENTO
      EXPORTING
        TEXTID = VALUE #( MSGID = ZCX_ORDEM_CARREGAMENTO=>ZCX_NAO_ENCONTRADO-MSGID
                          MSGNO = ZCX_ORDEM_CARREGAMENTO=>ZCX_NAO_ENCONTRADO-MSGNO )
        MSGTY  = 'E'
        MSGID  = ZCX_ORDEM_CARREGAMENTO=>ZCX_NAO_ENCONTRADO-MSGID
        MSGNO  = ZCX_ORDEM_CARREGAMENTO=>ZCX_NAO_ENCONTRADO-MSGNO.

  ENDMETHOD.


  METHOD zif_pesquisa~pesquisar.

    DATA: lc_filtro  TYPE zde_filtro_zsdt0001od,
          wa_retorno TYPE zde_zsdt0001od_alv,
          lc_retorno TYPE zde_zsdt0001od_alv_t.

    MOVE i_filtros TO lc_filtro.

    CLEAR: e_pesquisou, e_registros.

    SELECT ca~id_ordem           AS id_ordem,
           ca~nr_ordem           AS nr_ordem,
           ca~dt_emissao         AS dt_emissao,
           ca~dt_validade        AS dt_validade,
           ca~nr_safra           AS nr_safra,
           ca~id_bukrs           AS id_bukrs,
           en~butxt              AS butxt,
           ca~id_branch          AS id_branch,
           fn~name               AS name,
           ca~id_bukrs_ag        AS id_bukrs_ag,
           ef~butxt              AS butxt_ag,
           ca~id_branch_ag       AS id_branch_ag,
           ff~name               AS name_ag,
           ca~id_local_coleta    AS id_local_coleta,
           lc~name1              AS ds_local_coleta,
           ca~id_local_destino   AS id_local_destino,
           ld~name1              AS ds_local_destino,
           ca~id_local_descarga  AS id_local_descarga,
           la~name1              AS ds_local_descarga,
           ca~id_produto         AS id_produto,
           ma~maktx              AS ds_produto,
           ca~ds_placa_trator    AS ds_placa_trator,
           vt~proprietario       AS id_proprietario,
           pp~name1              AS ds_proprietario,
           ca~ds_placa_reboq_1   AS ds_placa_reboq_1,
           ca~ds_placa_reboq_2   AS ds_placa_reboq_2,
           ca~ds_placa_reboq_3   AS ds_placa_reboq_3,
           ca~id_motorista       AS id_motorista,
           mt~name1              AS ds_motorista,
           ca~tp_status          AS tp_status,
           ca~nr_peso_alvo       AS nr_peso_alvo,
           ca~nr_frete_comb      AS nr_frete_comb,
           ca~agente_frete       AS agente_frete "*-CS2021000253-26.04.2024-#59941-JT

      INTO TABLE @DATA(it_tab)
      FROM zsdt0001od AS ca
     INNER JOIN t001       AS en ON en~bukrs EQ ca~id_bukrs
     INNER JOIN j_1bbranch AS fn ON fn~bukrs EQ ca~id_bukrs AND fn~branch EQ ca~id_branch
      LEFT JOIN t001       AS ef ON ef~bukrs EQ ca~id_bukrs_ag
      LEFT JOIN j_1bbranch AS ff ON ff~bukrs EQ ca~id_bukrs_ag AND ff~branch EQ ca~id_branch_ag
      LEFT JOIN lfa1       AS lc ON lc~lifnr EQ ca~id_local_coleta
      LEFT JOIN lfa1       AS ld ON ld~lifnr EQ ca~id_local_destino
      LEFT JOIN kna1       AS la ON la~kunnr EQ ca~id_local_descarga
      LEFT JOIN zlest0002  AS vt ON vt~pc_veiculo EQ ca~ds_placa_trator
      LEFT JOIN lfa1       AS pp ON pp~lifnr EQ vt~proprietario
      LEFT JOIN lfa1       AS mt ON mt~lifnr EQ ca~id_motorista
      LEFT JOIN makt       AS ma ON ma~spras EQ @sy-langu AND ma~matnr EQ ca~id_produto
     WHERE ca~id_ordem           IN @lc_filtro-iidordem
       AND ca~nr_ordem           IN @lc_filtro-inrordem
       AND ca~dt_emissao         IN @lc_filtro-idtemiss
       AND ca~dt_validade        IN @lc_filtro-idvalida
       AND ca~nr_safra           IN @lc_filtro-inrsafra
       AND ca~id_bukrs           IN @lc_filtro-iidbukrs
       AND ca~id_branch          IN @lc_filtro-iidbranc
       AND ca~id_bukrs_ag        IN @lc_filtro-iidbukrg
       AND ca~id_branch_ag       IN @lc_filtro-iidbrang
       AND ca~id_local_coleta    IN @lc_filtro-iidcolet
       AND ca~id_local_destino   IN @lc_filtro-iiddesti
       AND ca~id_local_descarga  IN @lc_filtro-iiddesca
       AND ca~id_produto         IN @lc_filtro-iidprodu
       AND ca~ds_placa_trator    IN @lc_filtro-idstrato
       AND ca~ds_placa_reboq_1   IN @lc_filtro-idsrebo1
       AND ca~ds_placa_reboq_2   IN @lc_filtro-idsrebo2
       AND ca~ds_placa_reboq_3   IN @lc_filtro-idsrebo3
       AND ca~id_motorista       IN @lc_filtro-iidmotor
       AND ca~tp_status          IN @lc_filtro-itpstatu.

    LOOP AT it_tab INTO DATA(wa_tab).
      CLEAR: wa_retorno.
      wa_retorno-id_ordem          = wa_tab-id_ordem.
      wa_retorno-nr_ordem          = wa_tab-nr_ordem.
      wa_retorno-dt_emissao        = wa_tab-dt_emissao.
      wa_retorno-dt_validade       = wa_tab-dt_validade.
      wa_retorno-nr_safra          = wa_tab-nr_safra.
      wa_retorno-id_bukrs          = wa_tab-id_bukrs.
      wa_retorno-butxt             = wa_tab-butxt.
      wa_retorno-id_branch         = wa_tab-id_branch.
      wa_retorno-name              = wa_tab-name.
      wa_retorno-id_bukrs_ag       = wa_tab-id_bukrs_ag.
      wa_retorno-butxt_ag          = wa_tab-butxt_ag.
      wa_retorno-id_branch_ag      = wa_tab-id_branch_ag.
      wa_retorno-name_ag           = wa_tab-name_ag.
      wa_retorno-id_local_coleta   = wa_tab-id_local_coleta.
      wa_retorno-ds_local_coleta   = wa_tab-ds_local_coleta.
      wa_retorno-id_local_destino  = wa_tab-id_local_destino.
      wa_retorno-ds_local_destino  = wa_tab-ds_local_destino.
      wa_retorno-id_local_descarga = wa_tab-id_local_descarga.
      wa_retorno-ds_local_descarga = wa_tab-ds_local_descarga.
      wa_retorno-id_agent_frete    = wa_tab-id_branch_ag.
      wa_retorno-id_produto        = wa_tab-id_produto.
      wa_retorno-ds_produto        = wa_tab-ds_produto.
      wa_retorno-ds_placa_trator   = wa_tab-ds_placa_trator.
      wa_retorno-id_proprietario   = wa_tab-id_proprietario.
      wa_retorno-ds_proprietario   = wa_tab-ds_proprietario.
      wa_retorno-ds_placa_reboq_1  = wa_tab-ds_placa_reboq_1.
      wa_retorno-ds_placa_reboq_2  = wa_tab-ds_placa_reboq_2.
      wa_retorno-ds_placa_reboq_3  = wa_tab-ds_placa_reboq_3.
      wa_retorno-id_motorista      = wa_tab-id_motorista.
      wa_retorno-ds_motorista      = wa_tab-ds_motorista.
      wa_retorno-tp_status         = wa_tab-tp_status.
      wa_retorno-nr_peso_alvo      = wa_tab-nr_peso_alvo.
      wa_retorno-nr_frete_comb     = wa_tab-nr_frete_comb.
      wa_retorno-id_agent_frete_terceiro = wa_tab-agente_frete. "*-CS2021000253-26.04.2024-#59941-JT

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = wa_retorno-id_agent_frete
        IMPORTING
          output = wa_retorno-id_agent_frete.

      APPEND wa_retorno TO lc_retorno.

    ENDLOOP.

    CHECK lc_retorno IS NOT INITIAL.

    e_registros = lc_retorno.
    e_pesquisou = abap_true.

  ENDMETHOD.
ENDCLASS.
