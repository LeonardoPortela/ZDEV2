class ZCL_FERROVIARIO_SAIDA definition
  public
  inheriting from ZCL_FERROVIARIO
  create public .

public section.

  interfaces ZIF_CADASTRO .
  interfaces ZIF_PESQUISA .

  aliases CK_ALTEROU
    for ZIF_CADASTRO~CK_ALTEROU .
  aliases EXCLUIR_REGISTRO
    for ZIF_CADASTRO~EXCLUIR_REGISTRO .
  aliases GET_REGISTRO
    for ZIF_CADASTRO~GET_REGISTRO .
  aliases GRAVAR_REGISTRO
    for ZIF_CADASTRO~GRAVAR_REGISTRO .
  aliases LIMPAR_REGISTRO
    for ZIF_CADASTRO~LIMPAR_REGISTRO .
  aliases NOVO_REGISTRO
    for ZIF_CADASTRO~NOVO_REGISTRO .
  aliases SET_REGISTRO
    for ZIF_CADASTRO~SET_REGISTRO .
  aliases VALIDAR_EXCLUSAO
    for ZIF_CADASTRO~VALIDAR_EXCLUSAO .
  aliases VALIDAR_REGISTRO
    for ZIF_CADASTRO~VALIDAR_REGISTRO .
  aliases VALIDA_ATRIBUTO_ALTERAVEL
    for ZIF_CADASTRO~VALIDA_ATRIBUTO_ALTERAVEL .

  methods GET_SAIDA_ALV
    returning
      value(R_PROCESSO) type ZDE_FERRO_SAIDA .
  methods MONTA_PROCESSO
    importing
      !I_DCL type ZDCL optional
      !I_SERIE type ZSERIEDCL optional
      !I_ID_VAGAO type ZIDVAGAO optional
      !I_CNPJ type ZCNPJFERRO optional
      !I_NOTAS type ZDE_ZLEST0019_L2_30_T optional .
  methods SET_QT_SAIDA
    importing
      !I_BUKRS type BUKRS
      !I_BRANCH type J_1BBRANC_
      !I_NR_NF_PROPRIA type J_1BNFNUM9
      !I_SERIE_PROPRIA type J_1BSERIES
      !I_QT_SAIDA type ZDE_QT_SAIDA
    returning
      value(R_QT_SAIDA) type ZDE_QT_SAIDA
    raising
      ZCX_FERROVIARIO_SAIDA .
  methods SET_DT_SAIDA
    importing
      !I_DT_SAIDA type ZDE_DT_SAIDA .
  methods ADD_NOTA_FISCAL
    importing
      !I_DOCNUM type J_1BDOCNUM
    returning
      value(R_NOTA_SAIDA) type ZDE_RET_PESQ_FERRO_SAIDA
    raising
      ZCX_FERROVIARIO_SAIDA .
  methods SET_VAGAO_SAIDA
    changing
      !I_INFO_SAIDA type ZDE_FERRO_SAIDA_LAC .
  methods EXC_NOTA_FISCAL
    importing
      !I_DOCNUM type J_1BDOCNUM
    returning
      value(R_QT_SAIDA) type ZDE_QT_SAIDA .
  methods SET_CK_REGISTRO_NOVO
    importing
      !I_CK_REGISTRO_NOVO type CHAR01 .
protected section.
private section.

  data VAGAO_SAIDA type ZLEST0019_L2_20 .
  data PESO_NOTAS_SAIDAS type ZDE_ZLEST0019_L2_30_T .
  data NOTAS_TERCEIRO type ZDE_ZLEST0041_T .
  data PROCESSO type ZDE_FERRO_SAIDA .
  data CK_REGISTRO_NOVO type CHAR01 .
ENDCLASS.



CLASS ZCL_FERROVIARIO_SAIDA IMPLEMENTATION.


  METHOD ADD_NOTA_FISCAL.

    DATA: LC_FILTRO  TYPE ZDE_FILTRO_FERRO_SAIDA,
          WA_IDOCNUM TYPE ZDE_J_1BDOCNUM_RANGES,
          LC_RETORNO TYPE ZDE_RET_PESQ_FERRO_SAIDA_T,
          W_NOTAS	   TYPE ZLEST0019_L2_30,
          I_NOTAS	   TYPE ZDE_ZLEST0019_L2_30_T,
          I_DCL	     TYPE ZDCL,
          I_SERIE	   TYPE ZSERIEDCL,
          I_ID_VAGAO TYPE ZIDVAGAO,
          I_CNPJ     TYPE ZCNPJFERRO.

    IF ME->VAGAO_SAIDA-DCL IS INITIAL OR
       ME->VAGAO_SAIDA-SERIEDCL IS INITIAL OR
       ME->VAGAO_SAIDA-CNPJFERRO IS INITIAL OR
       ME->VAGAO_SAIDA-IDVAGAO IS INITIAL.

      RAISE EXCEPTION TYPE ZCX_FERROVIARIO_SAIDA
        EXPORTING
          TEXTID = VALUE #( MSGID = ZCX_FERROVIARIO_SAIDA=>ZCX_DADOS_DCL_VAGAO-MSGID
                            MSGNO = ZCX_FERROVIARIO_SAIDA=>ZCX_DADOS_DCL_VAGAO-MSGNO )
          MSGID  = ZCX_FERROVIARIO_SAIDA=>ZCX_DADOS_DCL_VAGAO-MSGID
          MSGNO  = ZCX_FERROVIARIO_SAIDA=>ZCX_DADOS_DCL_VAGAO-MSGNO
          MSGTY  = 'E'.
    ENDIF.

    READ TABLE ME->PESO_NOTAS_SAIDAS WITH KEY DOCNUM = I_DOCNUM INTO DATA(WA_SAIDA).
    IF SY-SUBRC IS INITIAL.

      SELECT SINGLE * INTO @DATA(WA_J_1BNFDOC)
        FROM J_1BNFDOC
       WHERE DOCNUM EQ @I_DOCNUM.

      RAISE EXCEPTION TYPE ZCX_FERROVIARIO_SAIDA
        EXPORTING
          TEXTID = VALUE #( MSGID = ZCX_FERROVIARIO_SAIDA=>ZCX_ESTA_NO_PROCESSO-MSGID
                            MSGNO = ZCX_FERROVIARIO_SAIDA=>ZCX_ESTA_NO_PROCESSO-MSGNO
                            ATTR1 = CONV #( WA_J_1BNFDOC-NFENUM )
                            ATTR2 = CONV #( WA_J_1BNFDOC-SERIES ) )
          MSGID  = ZCX_FERROVIARIO_SAIDA=>ZCX_ESTA_NO_PROCESSO-MSGID
          MSGNO  = ZCX_FERROVIARIO_SAIDA=>ZCX_ESTA_NO_PROCESSO-MSGNO
          MSGTY  = 'E'
          MSGV1  = CONV #( WA_J_1BNFDOC-NFENUM )
          MSGV2  = CONV #( WA_J_1BNFDOC-SERIES ).
    ENDIF.

    WA_IDOCNUM-SIGN   = 'I'.
    WA_IDOCNUM-OPTION = 'EQ'.
    WA_IDOCNUM-HIGH   = I_DOCNUM.
    WA_IDOCNUM-LOW   = I_DOCNUM.
    APPEND WA_IDOCNUM TO LC_FILTRO-IDOCNUM.

    IF ME->ZIF_PESQUISA~PESQUISAR( EXPORTING I_FILTROS = LC_FILTRO IMPORTING E_REGISTROS = LC_RETORNO ) EQ ABAP_TRUE.

      READ TABLE LC_RETORNO INDEX 1 INTO DATA(WA_RETORNO).

      CLEAR: I_NOTAS.

      LOOP AT ME->PESO_NOTAS_SAIDAS INTO DATA(WA_NOTAS_SAIDAS).
        CLEAR: W_NOTAS.
        W_NOTAS-BRANCH          = WA_NOTAS_SAIDAS-BRANCH.
        W_NOTAS-BUKRS           = WA_NOTAS_SAIDAS-BUKRS.
        W_NOTAS-NFENUM          = WA_NOTAS_SAIDAS-NFENUM.
        W_NOTAS-COD_FORNECEDOR  = WA_NOTAS_SAIDAS-COD_FORNECEDOR.
        W_NOTAS-DOCNUM          = WA_NOTAS_SAIDAS-DOCNUM.
        W_NOTAS-CNPJCLIENTE     = WA_NOTAS_SAIDAS-CNPJCLIENTE.
        W_NOTAS-PESODVAGAO      = WA_NOTAS_SAIDAS-PESODVAGAO.
        APPEND W_NOTAS TO I_NOTAS.

        SELECT SINGLE * INTO @DATA(WA_ZLEST0039_TRANSP_01)
          FROM ZLEST0039
         WHERE DOCNUM EQ @WA_NOTAS_SAIDAS-DOCNUM.

      ENDLOOP.

      IF WA_ZLEST0039_TRANSP_01 IS NOT INITIAL.

        SELECT SINGLE * INTO @DATA(WA_ZLEST0039_TRANSP_02)
          FROM ZLEST0039
         WHERE DOCNUM EQ @WA_RETORNO-DOCNUM.

        IF WA_ZLEST0039_TRANSP_01-TRANSB_EFETIVO IS NOT INITIAL.
          WA_ZLEST0039_TRANSP_01-PONTOTRANSB = WA_ZLEST0039_TRANSP_01-TRANSB_EFETIVO.
        ENDIF.

        IF WA_ZLEST0039_TRANSP_02-TRANSB_EFETIVO IS NOT INITIAL.
          WA_ZLEST0039_TRANSP_02-PONTOTRANSB = WA_ZLEST0039_TRANSP_02-TRANSB_EFETIVO.
        ENDIF.

        IF WA_ZLEST0039_TRANSP_02-PONTOTRANSB NE WA_ZLEST0039_TRANSP_01-PONTOTRANSB.
          RAISE EXCEPTION TYPE ZCX_FERROVIARIO_SAIDA
            EXPORTING
              TEXTID = VALUE #( MSGID = ZCX_FERROVIARIO_SAIDA=>ZCX_ERRO_TRANSBORDO-MSGID
                                MSGNO = ZCX_FERROVIARIO_SAIDA=>ZCX_ERRO_TRANSBORDO-MSGNO
                                ATTR1 = CONV #( ME->VAGAO_SAIDA-DCL )
                                ATTR2 = CONV #( ME->VAGAO_SAIDA-IDVAGAO ) )
              MSGID  = ZCX_FERROVIARIO_SAIDA=>ZCX_ERRO_TRANSBORDO-MSGID
              MSGNO  = ZCX_FERROVIARIO_SAIDA=>ZCX_ERRO_TRANSBORDO-MSGNO
              MSGTY  = 'E'
              MSGV1  = CONV #( ME->VAGAO_SAIDA-DCL )
              MSGV2  = CONV #( ME->VAGAO_SAIDA-IDVAGAO ).
        ENDIF.
      ENDIF.

      W_NOTAS-BRANCH          = WA_RETORNO-BRANCH.
      W_NOTAS-BUKRS           = WA_RETORNO-BUKRS.
      W_NOTAS-NFENUM          = WA_RETORNO-NR_NF_PROPRIA.
      W_NOTAS-COD_FORNECEDOR  = WA_RETORNO-COD_CLIENTE.
      W_NOTAS-DOCNUM          = WA_RETORNO-DOCNUM.
      W_NOTAS-CNPJCLIENTE     = WA_RETORNO-CNPJCLIENTE.
      APPEND W_NOTAS TO I_NOTAS.

      I_DCL      = ME->VAGAO_SAIDA-DCL.
      I_SERIE    = ME->VAGAO_SAIDA-SERIEDCL.
      I_ID_VAGAO = ME->VAGAO_SAIDA-IDVAGAO.
      I_CNPJ     = ME->VAGAO_SAIDA-CNPJFERRO.

      ME->MONTA_PROCESSO(
        EXPORTING
          I_DCL      = I_DCL
          I_SERIE    = I_SERIE
          I_ID_VAGAO = I_ID_VAGAO
          I_CNPJ     = I_CNPJ
          I_NOTAS    = I_NOTAS ).

      R_NOTA_SAIDA              = WA_RETORNO.
      R_NOTA_SAIDA-DCL          = ME->VAGAO_SAIDA-DCL.
      R_NOTA_SAIDA-IDVAGAO      = ME->VAGAO_SAIDA-IDVAGAO.
      R_NOTA_SAIDA-CNPJFERRO    = ME->VAGAO_SAIDA-CNPJFERRO.
      R_NOTA_SAIDA-SERIEDCL     = ME->VAGAO_SAIDA-SERIEDCL.
      R_NOTA_SAIDA-EMIT_CNPJ    = ME->VAGAO_SAIDA-CNPJFERRO.
      R_NOTA_SAIDA-EMIT_RSOCIAL = ME->VAGAO_SAIDA-NOMEMPFERRO.
      ME->CK_ALTEROU = ABAP_TRUE.

    ELSE.
      RAISE EXCEPTION TYPE ZCX_FERROVIARIO_SAIDA
        EXPORTING
          TEXTID = VALUE #( MSGID = ZCX_FERROVIARIO_SAIDA=>ZCX_DOC_NAO_ENCONTRADO-MSGID
                            MSGNO = ZCX_FERROVIARIO_SAIDA=>ZCX_DOC_NAO_ENCONTRADO-MSGNO
                            ATTR1 = CONV #( I_DOCNUM ) )
          MSGID  = ZCX_FERROVIARIO_SAIDA=>ZCX_DOC_NAO_ENCONTRADO-MSGID
          MSGNO  = ZCX_FERROVIARIO_SAIDA=>ZCX_DOC_NAO_ENCONTRADO-MSGNO
          MSGTY  = 'E'
          MSGV1  = CONV #( I_DOCNUM ).
    ENDIF.

  ENDMETHOD.


  METHOD EXC_NOTA_FISCAL.

    READ TABLE ME->PROCESSO-NOTAS INTO DATA(WA_NOTA) WITH KEY DOCNUM = I_DOCNUM.
    IF SY-SUBRC IS INITIAL.

      WA_NOTA-PESO_SAIDA = 0.

      R_QT_SAIDA = ME->SET_QT_SAIDA(
          I_BUKRS         = WA_NOTA-BUKRS
          I_BRANCH        = WA_NOTA-BRANCH
          I_NR_NF_PROPRIA = WA_NOTA-NR_NF_PROPRIA
          I_SERIE_PROPRIA = WA_NOTA-SERIE_PROPRIA
          I_QT_SAIDA      = WA_NOTA-PESO_SAIDA ).

      DELETE ME->PROCESSO-NOTAS WHERE DOCNUM EQ I_DOCNUM.
      DELETE ME->PESO_NOTAS_SAIDAS WHERE DOCNUM EQ I_DOCNUM.

    ENDIF.

  ENDMETHOD.


  METHOD GET_SAIDA_ALV.
    R_PROCESSO = ME->PROCESSO.
  ENDMETHOD.


  METHOD monta_processo.

    DATA: wa_notas     TYPE zde_ferro_saida_notas,
          wa_zlest0041 TYPE zlest0041.

    CLEAR: me->processo.

    IF i_dcl IS NOT INITIAL AND
       i_serie IS NOT INITIAL AND
       i_id_vagao IS NOT INITIAL AND
       i_cnpj  IS NOT INITIAL.

      CLEAR: me->vagao_saida,
             me->notas_terceiro,
             me->peso_notas_saidas.

      me->vagao_saida-dcl        = i_dcl.
      me->vagao_saida-idvagao    = i_id_vagao.
      me->vagao_saida-cnpjferro  = i_cnpj.
      me->vagao_saida-seriedcl   = i_serie.
      me->vagao_saida-pesovagao  = 0.
      CONCATENATE me->vagao_saida-dcl me->vagao_saida-idvagao INTO me->vagao_saida-chave.

      IF i_notas IS NOT INITIAL.
        SELECT *
          FROM j_1bbranch
          INTO TABLE @DATA(it_branch)
           FOR ALL ENTRIES IN @i_notas
         WHERE bukrs  EQ @i_notas-bukrs
           AND branch EQ @i_notas-branch.
        SORT it_branch BY bukrs branch.
      ENDIF.

      "ZDE_ZLEST0019_L2_30_T
      LOOP AT i_notas INTO DATA(wa_nota).

        "Busca Filial
        READ TABLE it_branch INTO DATA(wa_branch) WITH KEY bukrs = wa_nota-bukrs branch = wa_nota-branch BINARY SEARCH.

        "Verificar Remessa por conta e ordem de terceiro
        SELECT SINGLE * INTO @DATA(wa_lfa1) FROM lfa1 WHERE stcd1 EQ @wa_nota-cnpjcliente.

        IF sy-subrc IS INITIAL.

          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
            EXPORTING
              input  = wa_nota-nfenum
            IMPORTING
              output = wa_nota-nfenum.

          SELECT SINGLE * INTO wa_zlest0041
            FROM zlest0041
           WHERE nr_nf       EQ wa_nota-nfenum
             AND cod_cliente EQ wa_lfa1-lifnr.

          IF sy-subrc IS INITIAL.
            wa_nota-docnum         = wa_zlest0041-docnum.
            wa_nota-nr_nf_terceiro = wa_zlest0041-nr_nf.
          ENDIF.
        ENDIF.

        IF wa_nota-docnum IS INITIAL AND wa_branch IS NOT INITIAL.
          SELECT SINGLE * INTO @DATA(wa_nfedoc)
            FROM j_1bnfdoc
           WHERE nfenum EQ @wa_nota-nfenum
             AND bukrs  EQ @wa_branch-bukrs
             AND branch EQ @wa_branch-branch
             AND cancel NE @abap_true
             AND form   NE @space.
        ELSEIF wa_nota-docnum IS NOT INITIAL.
          SELECT SINGLE * INTO wa_nfedoc
            FROM j_1bnfdoc
           WHERE docnum EQ wa_nota-docnum
             AND cancel NE abap_true
             AND form   NE space.
        ENDIF.

        IF wa_nfedoc IS NOT INITIAL.
          SELECT SINGLE * INTO @DATA(wa_j_1bnflin)
            FROM j_1bnflin
           WHERE docnum EQ @wa_nfedoc-docnum.

          SELECT SINGLE * INTO @DATA(wa_zlest0039)
            FROM zlest0039
           WHERE docnum EQ @wa_nfedoc-docnum.

          wa_nota-bukrs  = wa_nfedoc-bukrs.
          wa_nota-branch = wa_nfedoc-branch.
          wa_nota-docnum = wa_nfedoc-docnum.
          wa_nota-nfenum = wa_nfedoc-nfenum.
          wa_nota-matnr  = wa_j_1bnflin-matnr.
          wa_nota-pesonf = wa_zlest0039-pesosaida.
          CONCATENATE wa_nota-bukrs wa_nota-branch wa_nota-nfenum INTO me->vagao_saida-chave.
          APPEND wa_nota TO me->peso_notas_saidas.
          CLEAR: wa_nfedoc.
        ENDIF.
      ENDLOOP.

      "Buscar Informações de Entrada de Peso do Documento
      SELECT nn~docnum,
             vg~cnpjferro,
             vg~nomempferro,
             vg~dcl,
             vg~seriedcl,
             vg~idvagao,
             vg~id_refkey,
             vg~id_zlest0019,
             vg~pesovagao	   AS peso_saida_vagao,
             vg~dtadecarga   AS data_saida_vagao,
             nn~pesodvagao   AS peso_saida_nota,
             nn~id_refkey    AS n_id_refkey ,
             nn~id_zlest0019 AS n_id_zlest0019
        INTO TABLE @DATA(it_l2)
        FROM zlest0019_l2_30 AS nn
       INNER JOIN zlest0019_l2_20 AS vg ON vg~id_refkey EQ nn~id_refkey AND vg~dcl EQ nn~dcl AND vg~cnpjferro EQ nn~cnpjferro
       WHERE vg~dcl       EQ @me->vagao_saida-dcl
         AND vg~idvagao   EQ @me->vagao_saida-idvagao
         AND vg~cnpjferro EQ @me->vagao_saida-cnpjferro.

      SORT it_l2 BY docnum.

      LOOP AT it_l2 INTO DATA(wa_l2).
        READ TABLE me->peso_notas_saidas ASSIGNING FIELD-SYMBOL(<fs_nota>) WITH KEY docnum = wa_l2-docnum.
        IF sy-subrc IS INITIAL.
          <fs_nota>-pesodvagao         = wa_l2-peso_saida_nota.
          <fs_nota>-id_refkey          = wa_l2-n_id_refkey.
          <fs_nota>-id_zlest0019       = wa_l2-n_id_zlest0019.
          me->vagao_saida-dtadecarga   = wa_l2-data_saida_vagao.
          me->vagao_saida-pesovagao    = wa_l2-peso_saida_vagao.
          me->vagao_saida-id_refkey    = wa_l2-id_refkey.
          me->vagao_saida-id_zlest0019 = wa_l2-id_zlest0019.
          me->set_id_refkey( i_id_refkey = wa_l2-id_refkey ).
        ELSE.
          SELECT SINGLE * INTO @DATA(wa_zlest0019_l2_30)
            FROM zlest0019_l2_30
           WHERE docnum    EQ @wa_l2-docnum
             AND dcl       EQ @wa_l2-dcl
             AND cnpjferro EQ @wa_l2-cnpjferro.
          IF sy-subrc IS INITIAL.
            APPEND wa_zlest0019_l2_30 TO me->peso_notas_saidas.
          ENDIF.
        ENDIF.
      ENDLOOP.
    ENDIF.

    CHECK me->vagao_saida IS NOT INITIAL.
    CHECK me->peso_notas_saidas IS NOT INITIAL.

    DATA(it_notas_saidas) = me->peso_notas_saidas[].
    DELETE it_notas_saidas WHERE docnum IS INITIAL.

    SELECT * INTO TABLE @DATA(it_zlest0039)
      FROM zlest0039
       FOR ALL ENTRIES IN @it_notas_saidas
     WHERE docnum EQ @it_notas_saidas-docnum.

    SORT it_zlest0039 BY docnum.

    IF it_zlest0039 IS NOT INITIAL.
      SELECT * INTO TABLE @DATA(it_zsdt0001)
        FROM zsdt0001
         FOR ALL ENTRIES IN @it_zlest0039
       WHERE doc_rem      EQ @it_zlest0039-vbeln
         AND tp_movimento EQ 'S'.
      SORT it_zsdt0001 BY doc_rem.
    ENDIF.

    me->vagao_saida-pesovagao = 0.
    LOOP AT me->peso_notas_saidas INTO DATA(wa_peso_nota).
      ADD wa_peso_nota-pesodvagao TO me->vagao_saida-pesovagao.
    ENDLOOP.

    LOOP AT me->peso_notas_saidas INTO DATA(wa_nf_cte) WHERE nr_nf_terceiro IS NOT INITIAL.
      wa_zlest0041-nr_nf       = wa_nf_cte-nr_nf_terceiro.
      wa_zlest0041-cod_cliente = wa_nf_cte-cod_fornecedor.

      SELECT SINGLE * INTO wa_zlest0041
        FROM zlest0041
       WHERE nr_nf       EQ wa_zlest0041-nr_nf
         AND cod_cliente EQ wa_zlest0041-cod_cliente.

      IF sy-subrc IS INITIAL.
        APPEND wa_zlest0041 TO me->notas_terceiro.
      ENDIF.

      ADD wa_nf_cte-pesodvagao TO me->vagao_saida-pesovagao.
    ENDLOOP.
    SORT me->notas_terceiro BY nr_nf cod_cliente nr_nf_propria.

    me->processo-dcl        = me->vagao_saida-dcl.
    me->processo-idvagao    = me->vagao_saida-idvagao.
    me->processo-emit_cnpj  = me->vagao_saida-cnpjferro.
    me->processo-seriedcl   = me->vagao_saida-seriedcl.
    SELECT SINGLE lifnr INTO me->processo-lifnr FROM lfa1 WHERE stcd1 EQ me->vagao_saida-cnpjferro.
    me->processo-dtasaida   = me->vagao_saida-dtadecarga.
    me->processo-peso_total = me->vagao_saida-pesovagao.

    IF me->peso_notas_saidas IS NOT INITIAL.
      SELECT * INTO TABLE @DATA(it_j_1bnfdoc)
        FROM j_1bnfdoc
         FOR ALL ENTRIES IN @me->peso_notas_saidas
       WHERE docnum EQ @me->peso_notas_saidas-docnum.

      SORT it_j_1bnfdoc BY docnum.
    ENDIF.

    LOOP AT me->peso_notas_saidas INTO DATA(wa_saida).

      wa_notas-bukrs           = wa_saida-bukrs.
      wa_notas-branch          = wa_saida-branch.
      wa_notas-nr_nf_propria   = wa_saida-nfenum.

      READ TABLE it_j_1bnfdoc WITH KEY docnum = wa_saida-docnum INTO DATA(wa_j_1bnfdoc) BINARY SEARCH.
      IF sy-subrc IS INITIAL.
        wa_notas-serie_propria = wa_j_1bnfdoc-series.
        wa_notas-docnum        = wa_j_1bnfdoc-docnum.
      ENDIF.

      wa_notas-peso_origem     = wa_saida-pesonf.
      wa_notas-peso_saida      = wa_saida-pesodvagao.
      wa_notas-nr_nf           = wa_saida-nr_nf_terceiro.
      wa_notas-cod_cliente     = wa_saida-cod_fornecedor.

      READ TABLE me->notas_terceiro INTO DATA(wa_remessa)
      WITH KEY nr_nf         = wa_saida-nr_nf_terceiro
               cod_cliente   = wa_saida-cod_fornecedor
               nr_nf_propria = wa_saida-nfenum BINARY SEARCH.
      IF sy-subrc IS INITIAL.
        wa_notas-centro_comprador = wa_remessa-centro_comprador.
        wa_notas-cod_material     = wa_remessa-cod_material.
        wa_notas-serie            = wa_remessa-serie.
        wa_notas-nr_nf            = wa_remessa-nr_nf.
        wa_notas-cod_cliente      = wa_remessa-cod_cliente.
        SELECT SINGLE stcd1 INTO wa_notas-cnpjcliente FROM lfa1 WHERE lifnr EQ wa_remessa-cod_cliente.
      ENDIF.

      IF wa_saida-docnum IS NOT INITIAL.
        "Busca Romaneio
        READ TABLE it_zlest0039 INTO wa_zlest0039 WITH KEY docnum = wa_saida-docnum BINARY SEARCH.
        IF sy-subrc IS INITIAL AND wa_zlest0039-vbeln IS NOT INITIAL.
          wa_notas-peso_origem = wa_zlest0039-pesosaida.
          wa_notas-cnpjcliente = wa_zlest0039-cnpj.
          READ TABLE it_zsdt0001 INTO DATA(wa_zsdt0001) WITH KEY doc_rem = wa_zlest0039-vbeln BINARY SEARCH.
          IF sy-subrc IS INITIAL.
            wa_notas-nr_romaneio      = wa_zsdt0001-nr_romaneio.
            wa_notas-ch_referencia    = wa_zsdt0001-ch_referencia.
            wa_notas-nr_safra         = wa_zsdt0001-nr_safra.
            wa_notas-nr_ticket        = wa_zsdt0001-nr_ticket.
            wa_notas-nr_perc_umidade  = wa_zsdt0001-nr_perc_umidade.
            wa_notas-nr_qtd_umidade   = wa_zsdt0001-nr_qtd_umidade.
            wa_notas-nr_perc_impureza = wa_zsdt0001-nr_perc_impureza.
            wa_notas-nr_qtd_impureza  = wa_zsdt0001-nr_qtd_impureza.
            wa_notas-nr_perc_avaria   = wa_zsdt0001-nr_perc_avaria.
            wa_notas-nr_qtd_avaria    = wa_zsdt0001-nr_qtd_avaria.
            wa_notas-nr_perc_ardido   = wa_zsdt0001-nr_perc_ardido.
            wa_notas-nr_qtd_ardido    = wa_zsdt0001-nr_qtd_ardido.
            wa_notas-nr_perc_quebra   = wa_zsdt0001-nr_perc_quebra.
            wa_notas-nr_qtd_quebra    = wa_zsdt0001-nr_qtd_quebra.
            wa_notas-nr_perc_esverd   = wa_zsdt0001-nr_perc_esverd.
            wa_notas-nr_qtd_esverd    = wa_zsdt0001-nr_qtd_esverd.
          ENDIF.
        ENDIF.
      ENDIF.
      APPEND wa_notas TO me->processo-notas.
      "SORT ME->PROCESSO-NOTAS BY BUKRS BRANCH NR_NF_PROPRIA SERIE_PROPRIA.
    ENDLOOP.

  ENDMETHOD.


  METHOD SET_CK_REGISTRO_NOVO.
    ME->CK_REGISTRO_NOVO = I_CK_REGISTRO_NOVO.
  ENDMETHOD.


  METHOD SET_DT_SAIDA.

    IF ME->PROCESSO-DTASAIDA NE I_DT_SAIDA.
      ME->CK_ALTEROU = ABAP_TRUE.
    ENDIF.

    ME->PROCESSO-DTASAIDA = I_DT_SAIDA.

    ME->VAGAO_SAIDA-DTADECARGA = I_DT_SAIDA.

  ENDMETHOD.


METHOD SET_QT_SAIDA.

  READ TABLE ME->PROCESSO-NOTAS ASSIGNING FIELD-SYMBOL(<DS_NOTA>)
  WITH KEY BUKRS         = I_BUKRS
           BRANCH        = I_BRANCH
           NR_NF_PROPRIA = I_NR_NF_PROPRIA
           SERIE_PROPRIA = I_SERIE_PROPRIA.

  IF SY-SUBRC IS NOT INITIAL.
    RAISE EXCEPTION TYPE ZCX_FERROVIARIO_SAIDA
      EXPORTING
        TEXTID = VALUE #( MSGID = ZCX_FERROVIARIO_SAIDA=>ZCX_NOTA_NAO_ENCONTRADA-MSGID
                          MSGNO = ZCX_FERROVIARIO_SAIDA=>ZCX_NOTA_NAO_ENCONTRADA-MSGNO
                          ATTR1 = CONV #( I_NR_NF_PROPRIA )
                          ATTR2 = CONV #( I_SERIE_PROPRIA ) )
        MSGID  = ZCX_FERROVIARIO_SAIDA=>ZCX_NOTA_NAO_ENCONTRADA-MSGID
        MSGTY  = 'E'
        MSGNO  = ZCX_FERROVIARIO_SAIDA=>ZCX_NOTA_NAO_ENCONTRADA-MSGNO
        MSGV1  = CONV #( I_NR_NF_PROPRIA )
        MSGV2  = CONV #( I_SERIE_PROPRIA ).
  ENDIF.

  IF <DS_NOTA>-PESO_SAIDA NE I_QT_SAIDA.
    ME->CK_ALTEROU = ABAP_TRUE.
  ENDIF.

  <DS_NOTA>-PESO_SAIDA = I_QT_SAIDA.

  READ TABLE ME->PESO_NOTAS_SAIDAS ASSIGNING FIELD-SYMBOL(<FS_NOTA>)
  WITH KEY BUKRS  = I_BUKRS
           BRANCH = I_BRANCH
           NFENUM = I_NR_NF_PROPRIA.

  IF SY-SUBRC IS INITIAL.
    <FS_NOTA>-PESODVAGAO = I_QT_SAIDA.
  ENDIF.

  "Totaliza Entrada
  ME->VAGAO_SAIDA-PESOVAGAO	= 0.
  LOOP AT ME->PESO_NOTAS_SAIDAS INTO DATA(WA_SAIDA).
    ADD WA_SAIDA-PESODVAGAO TO ME->VAGAO_SAIDA-PESOVAGAO.
  ENDLOOP.

  R_QT_SAIDA = ME->VAGAO_SAIDA-PESOVAGAO.

ENDMETHOD.


  METHOD SET_VAGAO_SAIDA.

    IF I_INFO_SAIDA-DCL NE ME->VAGAO_SAIDA-DCL.
      ME->CK_ALTEROU = ABAP_TRUE.
    ENDIF.

    IF I_INFO_SAIDA-IDVAGAO NE ME->VAGAO_SAIDA-IDVAGAO.
      ME->CK_ALTEROU = ABAP_TRUE.
    ENDIF.

    IF I_INFO_SAIDA-LIFNR IS NOT INITIAL.
      SELECT SINGLE * INTO @DATA(WA_LFA1)
        FROM LFA1
       WHERE LIFNR EQ @I_INFO_SAIDA-LIFNR.
    ELSEIF I_INFO_SAIDA-EMIT_CNPJ IS NOT INITIAL.
      SELECT SINGLE * INTO WA_LFA1
        FROM LFA1
       WHERE STCD1 EQ I_INFO_SAIDA-EMIT_CNPJ.
    ENDIF.

    IF WA_LFA1-STCD1 NE ME->VAGAO_SAIDA-CNPJFERRO.
      ME->CK_ALTEROU = ABAP_TRUE.
    ENDIF.

    IF I_INFO_SAIDA-SERIEDCL NE ME->VAGAO_SAIDA-SERIEDCL.
      ME->CK_ALTEROU = ABAP_TRUE.
    ENDIF.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        INPUT  = I_INFO_SAIDA-DCL
      IMPORTING
        OUTPUT = I_INFO_SAIDA-DCL.

    ME->VAGAO_SAIDA-DCL         = I_INFO_SAIDA-DCL.
    ME->VAGAO_SAIDA-IDVAGAO     = I_INFO_SAIDA-IDVAGAO.
    ME->VAGAO_SAIDA-CNPJFERRO   = WA_LFA1-STCD1.
    ME->VAGAO_SAIDA-SERIEDCL    = I_INFO_SAIDA-SERIEDCL.
    ME->VAGAO_SAIDA-PESOVAGAO   = 0.
    ME->VAGAO_SAIDA-NOMEMPFERRO = WA_LFA1-NAME1.
    ME->VAGAO_SAIDA-DTADECARGA  = I_INFO_SAIDA-DTASAIDA.
    I_INFO_SAIDA-NAME1          = WA_LFA1-NAME1.
    I_INFO_SAIDA-PESO_TOTAL     = 0.

    CONCATENATE ME->VAGAO_SAIDA-DCL ME->VAGAO_SAIDA-IDVAGAO INTO ME->VAGAO_SAIDA-CHAVE.

    LOOP AT ME->PESO_NOTAS_SAIDAS INTO DATA(WA_PESO_NOTAS_SAIDAS).
      ADD WA_PESO_NOTAS_SAIDAS-PESODVAGAO TO ME->VAGAO_SAIDA-PESOVAGAO.
      ADD WA_PESO_NOTAS_SAIDAS-PESODVAGAO TO I_INFO_SAIDA-PESO_TOTAL.
    ENDLOOP.

  ENDMETHOD.


  METHOD ZIF_CADASTRO~EXCLUIR_REGISTRO.

    I_EXCLUIU = ABAP_FALSE.

    IF ME->VALIDAR_EXCLUSAO( ) EQ ABAP_TRUE.

      "Informação do DCL/Vagão
      DELETE FROM ZLEST0019_L2_20
       WHERE ID_REFKEY  EQ ME->VAGAO_SAIDA-ID_REFKEY
         AND DCL        EQ ME->VAGAO_SAIDA-DCL
         AND CNPJFERRO  EQ ME->VAGAO_SAIDA-CNPJFERRO
         AND IDVAGAO    EQ ME->VAGAO_SAIDA-IDVAGAO.

      "Informação das Notas do DCL/Vagão
      DELETE FROM ZLEST0019_L2_30
       WHERE ID_REFKEY  EQ ME->VAGAO_SAIDA-ID_REFKEY
         AND DCL        EQ ME->VAGAO_SAIDA-DCL
         AND CNPJFERRO  EQ ME->VAGAO_SAIDA-CNPJFERRO.

      "Informação do DCL/Vagão das Notas do DCL/Vagão - Antiga
      DELETE FROM ZLEST0019
       WHERE IDINTER    EQ 'L2'
         AND ID_REFKEY  EQ ME->VAGAO_SAIDA-ID_REFKEY
         AND DCL        EQ ME->VAGAO_SAIDA-DCL
         AND CNPJFERRO  EQ ME->VAGAO_SAIDA-CNPJFERRO.

      COMMIT WORK.

      I_EXCLUIU = ABAP_TRUE.
      MESSAGE S006 WITH ME->VAGAO_SAIDA-DCL.
      ME->LIMPAR_REGISTRO( ).
    ENDIF.

  ENDMETHOD.


  METHOD ZIF_CADASTRO~GET_REGISTRO.
    MOVE-CORRESPONDING ME->VAGAO_SAIDA TO E_REGISTRO.
  ENDMETHOD.


  METHOD ZIF_CADASTRO~GRAVAR_REGISTRO.

    DATA: IT_TLINES    TYPE TABLE OF TLINE,
          WA_TLINES    TYPE TLINE,
          WA_HEADER    TYPE THEAD,
          WA_ZLEST0019 TYPE ZLEST0019,
          IT_ZLEST0019 TYPE TABLE OF ZLEST0019.

    I_GRAVOU = ABAP_FALSE.

    IF ME->CK_ALTEROU EQ ABAP_TRUE.

      IF ME->VALIDAR_REGISTRO( ) EQ ABAP_TRUE.

        IF ME->VAGAO_SAIDA-ID_ZLEST0019 IS INITIAL.
          "Unitário
          ME->VAGAO_SAIDA-ID_ZLEST0019 = ME->GET_NEW_ID_REFKEY( ).
        ENDIF.

        "ID para o Grupo
        ME->VAGAO_SAIDA-ID_REFKEY = ME->ID_REFKEY.

        LOOP AT ME->PESO_NOTAS_SAIDAS ASSIGNING FIELD-SYMBOL(<FS_SAIDA>).
          <FS_SAIDA>-ID_REFKEY = ME->ID_REFKEY.
          IF <FS_SAIDA>-ID_ZLEST0019 IS INITIAL.
            <FS_SAIDA>-ID_ZLEST0019 = ME->GET_NEW_ID_REFKEY( ).
          ENDIF.
          CONCATENATE <FS_SAIDA>-BUKRS <FS_SAIDA>-BRANCH INTO <FS_SAIDA>-CHAVE SEPARATED BY '-'.
          CONCATENATE <FS_SAIDA>-CHAVE '-' INTO <FS_SAIDA>-CHAVE.
          CONCATENATE <FS_SAIDA>-CHAVE <FS_SAIDA>-NFENUM <FS_SAIDA>-NFNUM INTO <FS_SAIDA>-CHAVE.
          <FS_SAIDA>-IDINTER    = 'L2'.
          <FS_SAIDA>-TP_MOVI    = 'S'.
          <FS_SAIDA>-TP_REG	    = '30'.
          <FS_SAIDA>-DCL        = ME->VAGAO_SAIDA-DCL.
          <FS_SAIDA>-SERIEDCL   = ME->VAGAO_SAIDA-SERIEDCL.
          <FS_SAIDA>-CNPJFERRO  = ME->VAGAO_SAIDA-CNPJFERRO.
          <FS_SAIDA>-IDVAGAO    = ME->VAGAO_SAIDA-IDVAGAO.
          <FS_SAIDA>-DTACHEGADA = ME->VAGAO_SAIDA-DTADECARGA.
          <FS_SAIDA>-ERDAT      = SY-DATUM.
          <FS_SAIDA>-ERZET      = SY-UZEIT.
          <FS_SAIDA>-UNAME      = SY-UNAME.
        ENDLOOP.

        ME->VAGAO_SAIDA-IDINTER	       = 'L2'.
        ME->VAGAO_SAIDA-TP_MOVI	       = 'S'.
        ME->VAGAO_SAIDA-TP_REG         = '20'.
        ME->VAGAO_SAIDA-ERDAT          = SY-DATUM.
        ME->VAGAO_SAIDA-ERZET          = SY-UZEIT.
        ME->VAGAO_SAIDA-UNAME          = SY-UNAME.
        CONCATENATE ME->VAGAO_SAIDA-DCL ME->VAGAO_SAIDA-IDVAGAO INTO ME->VAGAO_SAIDA-CHAVE.
        MODIFY ZLEST0019_L2_20 FROM ME->VAGAO_SAIDA.
        MODIFY ZLEST0019_L2_30 FROM TABLE ME->PESO_NOTAS_SAIDAS.

        CLEAR: WA_ZLEST0019, IT_ZLEST0019, IT_ZLEST0019[].
        WA_ZLEST0019-IDINTER        = 'L2'.
        WA_ZLEST0019-TP_MOVI        = 'S'.
        WA_ZLEST0019-TP_REG         = '20'.
        WA_ZLEST0019-CHAVE          = ME->VAGAO_SAIDA-CHAVE.
        WA_ZLEST0019-DCL            = ME->VAGAO_SAIDA-DCL.
        WA_ZLEST0019-SERIEDCL       = ME->VAGAO_SAIDA-SERIEDCL.
        WA_ZLEST0019-CNPJFERRO      = ME->VAGAO_SAIDA-CNPJFERRO.
        WA_ZLEST0019-NOMEMPFERRO    = ME->VAGAO_SAIDA-NOMEMPFERRO.
        WA_ZLEST0019-DTAENVIO       = ME->VAGAO_SAIDA-DTAENVIO.
        WA_ZLEST0019-HORAENVIO      = ME->VAGAO_SAIDA-HORAENVIO.
        WA_ZLEST0019-OBS            = ME->VAGAO_SAIDA-OBS.
        WA_ZLEST0019-IDVAGAO        = ME->VAGAO_SAIDA-IDVAGAO.
        WA_ZLEST0019-PESOVAGAO      = ME->VAGAO_SAIDA-PESOVAGAO.
        WA_ZLEST0019-DTADECARGA     = ME->VAGAO_SAIDA-DTADECARGA.
        WA_ZLEST0019-HORADESCARGA   = ME->VAGAO_SAIDA-HORADESCARGA.
        WA_ZLEST0019-CNPJCLIENTE    = ME->VAGAO_SAIDA-CNPJCLIENTE.
        WA_ZLEST0019-BUKRS          = ME->VAGAO_SAIDA-BUKRS.
        WA_ZLEST0019-BRANCH         = ME->VAGAO_SAIDA-BRANCH.
        WA_ZLEST0019-NFENUM         = ME->VAGAO_SAIDA-NFENUM.
        WA_ZLEST0019-NFNUM          = ME->VAGAO_SAIDA-NFNUM.
        WA_ZLEST0019-PESONF         = ME->VAGAO_SAIDA-PESONF.
        WA_ZLEST0019-PESODVAGAO     = ME->VAGAO_SAIDA-PESODVAGAO.
        WA_ZLEST0019-DTACHEGADA     = ME->VAGAO_SAIDA-DTACHEGADA.
        WA_ZLEST0019-PRODUTO        = ME->VAGAO_SAIDA-PRODUTO.
        WA_ZLEST0019-ERDAT          = SY-DATUM.
        WA_ZLEST0019-ERZET          = SY-UZEIT.
        WA_ZLEST0019-UNAME          = SY-UNAME.
        WA_ZLEST0019-NR_NF_TERCEIRO = ME->VAGAO_SAIDA-NR_NF_TERCEIRO.
        WA_ZLEST0019-COD_FORNECEDOR = ME->VAGAO_SAIDA-COD_FORNECEDOR.
        WA_ZLEST0019-ID_ZLEST0019   = ME->VAGAO_SAIDA-ID_ZLEST0019.
        WA_ZLEST0019-ID_REFKEY      = ME->VAGAO_SAIDA-ID_REFKEY.
        WA_ZLEST0019-STATUS_DUPLICA = ME->VAGAO_SAIDA-STATUS_DUPLICA.
        WA_ZLEST0019-OBSERVACAO     = ME->VAGAO_SAIDA-OBSERVACAO.
        APPEND WA_ZLEST0019 TO IT_ZLEST0019.

        LOOP AT ME->PESO_NOTAS_SAIDAS INTO DATA(WA_PESO_NOTAS_SAIDAS).
          CLEAR WA_ZLEST0019.
          WA_ZLEST0019-IDINTER        = WA_PESO_NOTAS_SAIDAS-IDINTER.
          WA_ZLEST0019-TP_MOVI        = WA_PESO_NOTAS_SAIDAS-TP_MOVI.
          WA_ZLEST0019-TP_REG         = WA_PESO_NOTAS_SAIDAS-TP_REG.
          WA_ZLEST0019-CHAVE          = WA_PESO_NOTAS_SAIDAS-CHAVE.
          WA_ZLEST0019-DCL            = WA_PESO_NOTAS_SAIDAS-DCL.
          WA_ZLEST0019-SERIEDCL       = WA_PESO_NOTAS_SAIDAS-SERIEDCL.
          WA_ZLEST0019-CNPJFERRO      = WA_PESO_NOTAS_SAIDAS-CNPJFERRO.
          WA_ZLEST0019-NOMEMPFERRO    = WA_PESO_NOTAS_SAIDAS-NOMEMPFERRO.
          WA_ZLEST0019-DTAENVIO       = WA_PESO_NOTAS_SAIDAS-DTAENVIO.
          WA_ZLEST0019-HORAENVIO      = WA_PESO_NOTAS_SAIDAS-HORAENVIO.
          WA_ZLEST0019-OBS            = WA_PESO_NOTAS_SAIDAS-OBS.
          WA_ZLEST0019-IDVAGAO        = WA_PESO_NOTAS_SAIDAS-IDVAGAO.
          WA_ZLEST0019-PESOVAGAO      = WA_PESO_NOTAS_SAIDAS-PESOVAGAO.
          WA_ZLEST0019-DTADECARGA     = WA_PESO_NOTAS_SAIDAS-DTADECARGA.
          WA_ZLEST0019-HORADESCARGA   = WA_PESO_NOTAS_SAIDAS-HORADESCARGA.
          WA_ZLEST0019-CNPJCLIENTE    = WA_PESO_NOTAS_SAIDAS-CNPJCLIENTE.
          WA_ZLEST0019-BUKRS          = WA_PESO_NOTAS_SAIDAS-BUKRS.
          WA_ZLEST0019-BRANCH         = WA_PESO_NOTAS_SAIDAS-BRANCH.
          WA_ZLEST0019-NFENUM         = WA_PESO_NOTAS_SAIDAS-NFENUM.
          WA_ZLEST0019-NFNUM          = WA_PESO_NOTAS_SAIDAS-NFNUM.
          WA_ZLEST0019-PESONF         = WA_PESO_NOTAS_SAIDAS-PESONF.
          WA_ZLEST0019-PESODVAGAO     = WA_PESO_NOTAS_SAIDAS-PESODVAGAO.
          WA_ZLEST0019-DTACHEGADA     = WA_PESO_NOTAS_SAIDAS-DTACHEGADA.
          WA_ZLEST0019-PRODUTO        = WA_PESO_NOTAS_SAIDAS-PRODUTO.
          WA_ZLEST0019-ERDAT          = SY-DATUM.
          WA_ZLEST0019-ERZET          = SY-UZEIT.
          WA_ZLEST0019-UNAME          = SY-UNAME.
          WA_ZLEST0019-NR_NF_TERCEIRO = WA_PESO_NOTAS_SAIDAS-NR_NF_TERCEIRO.
          WA_ZLEST0019-COD_FORNECEDOR = WA_PESO_NOTAS_SAIDAS-COD_FORNECEDOR.
          WA_ZLEST0019-ID_ZLEST0019   = WA_PESO_NOTAS_SAIDAS-ID_ZLEST0019.
          WA_ZLEST0019-ID_REFKEY      = WA_PESO_NOTAS_SAIDAS-ID_REFKEY.
          WA_ZLEST0019-STATUS_DUPLICA = WA_PESO_NOTAS_SAIDAS-STATUS_DUPLICA.
          WA_ZLEST0019-OBSERVACAO     = WA_PESO_NOTAS_SAIDAS-OBSERVACAO.
          WA_ZLEST0019-DOCNUM         = WA_PESO_NOTAS_SAIDAS-DOCNUM.
          WA_ZLEST0019-MATNR          = WA_PESO_NOTAS_SAIDAS-MATNR.
          APPEND WA_ZLEST0019 TO IT_ZLEST0019.
        ENDLOOP.
        MODIFY ZLEST0019 FROM TABLE IT_ZLEST0019.

        COMMIT WORK.
        ME->CK_ALTEROU = ABAP_FALSE.
        I_GRAVOU = ABAP_TRUE.
        MESSAGE S007 WITH ME->VAGAO_SAIDA-DCL.
      ENDIF.

    ENDIF.

  ENDMETHOD.


  METHOD ZIF_CADASTRO~LIMPAR_REGISTRO.
    CLEAR: ME->ID_REFKEY,
           ME->NOTAS_TERCEIRO,
           ME->PESO_NOTAS_SAIDAS,
           ME->PROCESSO,
           ME->VAGAO_SAIDA.
  ENDMETHOD.


  METHOD ZIF_CADASTRO~NOVO_REGISTRO.
    ME->LIMPAR_REGISTRO( ).
  ENDMETHOD.


  METHOD ZIF_CADASTRO~SET_REGISTRO.

    DATA: LC_ID_VAGAO(8),
          LC_TP_VAGAO(8),
          WA_ZLEST0041 TYPE ZLEST0041.

    ME->LIMPAR_REGISTRO( ).

    SELECT SINGLE * INTO ME->VAGAO_SAIDA FROM ZLEST0019_L2_20 WHERE ID_ZLEST0019 EQ I_ID_REGISTRO.

    IF SY-SUBRC IS INITIAL.

      SELECT *
        INTO TABLE ME->PESO_NOTAS_SAIDAS
        FROM ZLEST0019_L2_30
       WHERE DCL       EQ ME->VAGAO_SAIDA-DCL
         AND IDVAGAO   EQ ME->VAGAO_SAIDA-IDVAGAO
         AND ID_REFKEY EQ ME->VAGAO_SAIDA-ID_REFKEY.

      IF SY-SUBRC IS INITIAL.
        ME->MONTA_PROCESSO( I_DCL        = ME->VAGAO_SAIDA-DCL
                            I_ID_VAGAO   = ME->VAGAO_SAIDA-IDVAGAO
                            I_SERIE      = ME->VAGAO_SAIDA-SERIEDCL
                            I_CNPJ       = ME->VAGAO_SAIDA-CNPJFERRO
                            I_NOTAS      = ME->PESO_NOTAS_SAIDAS ).
      ENDIF.

    ENDIF.

    ME->CK_ALTEROU = ABAP_FALSE.

  ENDMETHOD.


  METHOD ZIF_CADASTRO~VALIDAR_EXCLUSAO.

    E_VALIDOU = ABAP_FALSE.

    SELECT SINGLE * INTO @DATA(WA_L3_20)
      FROM ZLEST0019_L3_20
     WHERE DCL     EQ @ME->VAGAO_SAIDA-DCL
       AND IDVAGAO EQ @ME->VAGAO_SAIDA-IDVAGAO.

    IF SY-SUBRC IS INITIAL.
      MESSAGE S017 WITH ME->VAGAO_SAIDA-DCL ME->VAGAO_SAIDA-IDVAGAO DISPLAY LIKE 'E'.
      EXIT.
    ENDIF.

    IF ME->GET_ID_REFKEY( ) IS INITIAL.
      MESSAGE S005 DISPLAY LIKE 'E'.
      EXIT.
    ENDIF.

    E_VALIDOU = ABAP_TRUE.

  ENDMETHOD.


  METHOD ZIF_CADASTRO~VALIDAR_REGISTRO.

    DATA: CK_ERRO TYPE CHAR01.

    CLEAR: CK_ERRO.

    E_VALIDOU = ABAP_FALSE.

    IF ME->ID_REFKEY IS INITIAL.
      MESSAGE S003 DISPLAY LIKE 'E'.
      EXIT.
    ENDIF.

    IF ME->VAGAO_SAIDA-DCL IS INITIAL.
      MESSAGE S009 DISPLAY LIKE 'E'.
      EXIT.
    ENDIF.

    IF ME->VAGAO_SAIDA-SERIEDCL IS INITIAL.
      MESSAGE S010 DISPLAY LIKE 'E'.
      EXIT.
    ENDIF.

    IF ME->VAGAO_SAIDA-CNPJFERRO IS INITIAL.
      MESSAGE S011 DISPLAY LIKE 'E'.
      EXIT.
    ENDIF.

    IF ME->VAGAO_SAIDA-IDVAGAO IS INITIAL.
      MESSAGE S012 DISPLAY LIKE 'E'.
      EXIT.
    ENDIF.

    IF ME->VAGAO_SAIDA-DTADECARGA IS INITIAL.
      MESSAGE S014 DISPLAY LIKE 'E'.
      EXIT.
    ENDIF.

    DESCRIBE TABLE ME->PESO_NOTAS_SAIDAS LINES DATA(QTD_LINHAS).

    IF QTD_LINHAS IS INITIAL.
      MESSAGE S013 DISPLAY LIKE 'E'.
      EXIT.
    ENDIF.

    "Valida Data de Saída do Vagão """""""""""""""""""""""""""""""""""""""""""""""""""""""
    SELECT * INTO TABLE @DATA(IT_ZLEST0039)
      FROM ZLEST0039
       FOR ALL ENTRIES IN @ME->PESO_NOTAS_SAIDAS
     WHERE DOCNUM EQ @ME->PESO_NOTAS_SAIDAS-DOCNUM.

    SORT IT_ZLEST0039 BY DOCNUM.

    LOOP AT ME->PESO_NOTAS_SAIDAS INTO DATA(WA_NOTAS).
      READ TABLE IT_ZLEST0039 INTO DATA(WA_ZLEST0039) WITH KEY  DOCNUM = WA_NOTAS-DOCNUM.
      IF ME->VAGAO_SAIDA-DTADECARGA LT WA_ZLEST0039-DATATRANSB.
        MESSAGE S015 WITH WA_ZLEST0039-DATATRANSB DISPLAY LIKE 'E'.
        CK_ERRO = ABAP_TRUE.
        EXIT.
      ENDIF.
      CLEAR: WA_ZLEST0039.
    ENDLOOP.

    CHECK CK_ERRO EQ ABAP_FALSE.

    "Valida Peso de Saída do Vagao """"""""""""""""""""""""""""""""""""""""""""""""""""""
    LOOP AT ME->PESO_NOTAS_SAIDAS INTO WA_NOTAS.

      SELECT SINGLE
             MM~DOCNUM,
             MM~PESOTRANSB,
             SUM( NT~PESODVAGAO ) AS PESO_VINCULADO
             INTO @DATA(WA_PESOS)
        FROM ZLEST0039 AS MM
        LEFT JOIN ZLEST0019_L2_30 AS NT ON NT~DOCNUM = MM~DOCNUM
       WHERE MM~DOCNUM EQ @WA_NOTAS-DOCNUM
       GROUP BY MM~DOCNUM, MM~PESOTRANSB.

      IF ( SY-SUBRC IS INITIAL ) AND WA_PESOS-PESOTRANSB LT ( WA_PESOS-PESO_VINCULADO + WA_NOTAS-PESODVAGAO ).
        MESSAGE S016 WITH WA_NOTAS-NFENUM WA_PESOS-PESOTRANSB DISPLAY LIKE 'E'.
        CK_ERRO = ABAP_TRUE.
        EXIT.
      ENDIF.
    ENDLOOP.

    CHECK CK_ERRO EQ ABAP_FALSE.

    IF ME->CK_REGISTRO_NOVO EQ ABAP_TRUE.

      SELECT SINGLE * INTO @DATA(WA_DCL)
        FROM ZLEST0019_L2_20
       WHERE DCL       EQ @ME->VAGAO_SAIDA-DCL
         AND IDVAGAO   EQ @ME->VAGAO_SAIDA-IDVAGAO
         AND CNPJFERRO EQ @ME->VAGAO_SAIDA-CNPJFERRO
         AND SERIEDCL  EQ @ME->VAGAO_SAIDA-SERIEDCL.

      IF SY-SUBRC IS INITIAL.
        MESSAGE S008 WITH ME->VAGAO_SAIDA-DCL ME->VAGAO_SAIDA-IDVAGAO DISPLAY LIKE 'E'.
        CK_ERRO = ABAP_TRUE.
        EXIT.
      ENDIF.

    ENDIF.
*    IF ME->VAGAO_SAIDA-DTADECARGA GT ME->VAGAO_CHEGADA-DTADECARGA.
*      MESSAGE S004 DISPLAY LIKE 'E'.
*      EXIT.
*    ENDIF.

    CHECK CK_ERRO EQ ABAP_FALSE.

    E_VALIDOU = ABAP_TRUE.

  ENDMETHOD.


  METHOD ZIF_CADASTRO~VALIDA_ATRIBUTO_ALTERAVEL.

    R_PERMITIDO = ABAP_TRUE.

  ENDMETHOD.


  METHOD zif_pesquisa~pesquisar.

    DATA: lc_filtro  TYPE zde_filtro_ferro_saida,
          wa_retorno TYPE zde_ret_pesq_ferro_saida,
          lc_retorno TYPE zde_ret_pesq_ferro_saida_t.

    DATA: rdocnum TYPE RANGE OF j_1bdocnum,
          wdocnum LIKE LINE OF rdocnum,
          lc_data TYPE erdat.

    lc_filtro = i_filtros.

    e_pesquisou = abap_false.

    CLEAR: rdocnum.

    "Limitar Notas ao DCL e Vagão Selecionado
    IF lc_filtro-iidvag IS NOT INITIAL OR lc_filtro-inrdcl IS NOT INITIAL.

      SELECT * INTO TABLE @DATA(it_zlest0019_l2_20)
        FROM zlest0019_l2_30
       WHERE dcl     IN @lc_filtro-inrdcl
         AND idvagao IN @lc_filtro-iidvag.

      IF sy-subrc IS INITIAL.

        SELECT * INTO TABLE @DATA(it_zlest0019_l2_30)
          FROM zlest0019_l2_30
           FOR ALL ENTRIES IN @it_zlest0019_l2_20
         WHERE id_refkey EQ @it_zlest0019_l2_20-id_refkey
           AND dcl       EQ @it_zlest0019_l2_20-dcl
           AND cnpjferro EQ @it_zlest0019_l2_20-cnpjferro
           AND docnum    NE @space.

        IF sy-subrc IS NOT INITIAL.
          EXIT.
        ENDIF.

      ELSE.
        EXIT.
      ENDIF.

      wdocnum-sign   = 'I'.
      wdocnum-option = 'EQ'.
      LOOP AT it_zlest0019_l2_30 INTO DATA(wa_l2_30).
        wdocnum-low    = wa_l2_30-docnum.
        wdocnum-high   = wa_l2_30-docnum.
        APPEND wdocnum TO rdocnum.
      ENDLOOP.

    ENDIF.

    "Pesquisa Notas
    SELECT * INTO TABLE @DATA(it_zlest0039)
      FROM zlest0039
     WHERE bukrs      IN @lc_filtro-ibukrs
       AND werks      IN @lc_filtro-ibranc
       AND nfenum     IN @lc_filtro-innota
       AND datasaida  IN @lc_filtro-idtsai
       AND datatransb NE @lc_data
       AND datatransb IN @lc_filtro-itrans
       AND docnum     IN @lc_filtro-idocnum
       AND docnum     IN @rdocnum.

      DELETE it_zlest0039 WHERE pontotransb IS INITIAL AND transb_efetivo IS INITIAL.


    CHECK it_zlest0039 IS NOT INITIAL.

    SELECT * INTO TABLE @DATA(it_zsdt0001)
      FROM zsdt0001
       FOR ALL ENTRIES IN @it_zlest0039
     WHERE doc_rem      EQ @it_zlest0039-vbeln
       AND tp_movimento EQ 'S'.

    SORT it_zsdt0001 BY doc_rem.

    SELECT * INTO TABLE @DATA(it_material)
      FROM makt
       FOR ALL ENTRIES IN @it_zlest0039
     WHERE spras EQ @sy-langu
       AND matnr EQ @it_zlest0039-matnr.

    SORT it_material BY matnr.

    "Remessa por conta e ordem de terceiro
    SELECT * INTO TABLE @DATA(it_zlest0041)
      FROM zlest0041
       FOR ALL ENTRIES IN @it_zlest0039
     WHERE docnum      EQ @it_zlest0039-docnum.

    SORT it_zlest0041 BY docnum.

    "Parceiros de Transbordo
    SELECT * INTO TABLE @DATA(it_transbordos)
      FROM kna1
      FOR ALL ENTRIES IN @it_zlest0039
     WHERE kunnr EQ @it_zlest0039-pontotransb.

    DATA(it_zlest0039_copia) = it_zlest0039[].
    DELETE it_zlest0039_copia WHERE transb_efetivo IS INITIAL.
    SORT it_zlest0039_copia BY transb_efetivo.
    DELETE ADJACENT DUPLICATES FROM it_zlest0039_copia COMPARING transb_efetivo.
    IF it_zlest0039_copia IS NOT INITIAL.
      SELECT * APPENDING TABLE it_transbordos
        FROM kna1
         FOR ALL ENTRIES IN it_zlest0039
       WHERE kunnr EQ it_zlest0039-pontotransb.
    ENDIF.

    SORT it_transbordos BY kunnr.

    "Buscar Informações de Entrada de Peso do Documento
    SELECT nn~docnum,
           vg~cnpjferro,
           vg~nomempferro,
           vg~dcl,
           vg~seriedcl,
           vg~idvagao,
           vg~pesovagao	 AS peso_saida_vagao,
           vg~dtadecarga AS data_saida_vagao,
           nn~pesodvagao AS peso_saida_nota
      INTO TABLE @DATA(it_l2)
      FROM zlest0019_l2_30 AS nn
     INNER JOIN zlest0019_l2_20 AS vg ON vg~id_refkey EQ nn~id_refkey AND vg~dcl EQ nn~dcl AND vg~cnpjferro EQ nn~cnpjferro
       FOR ALL ENTRIES IN @it_zlest0039
     WHERE nn~docnum   NE @space
       AND vg~dcl      IN @lc_filtro-inrdcl
       AND vg~idvagao  IN @lc_filtro-iidvag
       AND nn~docnum   EQ @it_zlest0039-docnum.

    SORT it_l2 BY docnum.

    LOOP AT it_zlest0039 INTO DATA(wa_zlest0039).

      CLEAR: wa_retorno.

      wa_retorno-bukrs           = wa_zlest0039-bukrs.
      wa_retorno-branch          = wa_zlest0039-werks.
      wa_retorno-nr_nf_propria   = wa_zlest0039-nfenum.
      wa_retorno-serie_propria   = wa_zlest0039-serie.
      wa_retorno-docnum          = wa_zlest0039-docnum.
      wa_retorno-peso_origem     = wa_zlest0039-pesosaida.
      wa_retorno-cod_material    = wa_zlest0039-matnr.

      IF wa_zlest0039-transb_efetivo IS NOT INITIAL.
        wa_retorno-pontotransb = wa_zlest0039-transb_efetivo.
      ELSE.
        wa_retorno-pontotransb = wa_zlest0039-pontotransb.
      ENDIF.

      READ TABLE it_transbordos INTO DATA(wa_transbordos) WITH KEY kunnr = wa_retorno-pontotransb BINARY SEARCH.
      IF sy-subrc IS INITIAL.
        wa_retorno-transb_rsocial = wa_transbordos-name1.
      ENDIF.

      READ TABLE it_zlest0041 INTO DATA(wa_zlest0041) WITH KEY docnum = wa_zlest0039-docnum BINARY SEARCH.
      IF sy-subrc IS INITIAL.
        wa_retorno-nr_nf            = wa_zlest0041-nr_nf.
        wa_retorno-cod_cliente      = wa_zlest0041-cod_cliente.
        SELECT SINGLE stcd1 INTO wa_retorno-cnpjcliente FROM lfa1 WHERE lifnr EQ wa_zlest0041-cod_cliente.
        wa_retorno-centro_comprador = wa_zlest0041-centro_comprador.
        wa_retorno-serie            = wa_zlest0041-serie.
      ELSE.
        wa_retorno-cnpjcliente      = wa_zlest0039-cnpj.
      ENDIF.

      CALL FUNCTION 'CONVERSION_EXIT_CGCBR_OUTPUT'
        EXPORTING
          input  = wa_retorno-emit_cnpj
        IMPORTING
          output = wa_retorno-emit_cnpj.

      IF wa_retorno-cod_material IS NOT INITIAL.
        READ TABLE it_material INTO DATA(wa_material) WITH KEY matnr = wa_retorno-cod_material BINARY SEARCH.
        IF sy-subrc IS INITIAL.
          wa_retorno-dsc_material = wa_material-maktx.
        ENDIF.
      ENDIF.

      IF wa_zlest0039-vbeln IS NOT INITIAL.
        READ TABLE it_zsdt0001 INTO DATA(wa_zsdt0001) WITH KEY doc_rem = wa_zlest0039-vbeln BINARY SEARCH.
        IF sy-subrc IS INITIAL.
          wa_retorno-nr_romaneio      = wa_zsdt0001-nr_romaneio.
          wa_retorno-ch_referencia    = wa_zsdt0001-ch_referencia.
          wa_retorno-nr_safra         = wa_zsdt0001-nr_safra.
          wa_retorno-nr_ticket        = wa_zsdt0001-nr_ticket.
          wa_retorno-nr_perc_umidade  = wa_zsdt0001-nr_perc_umidade.
          wa_retorno-nr_qtd_umidade   = wa_zsdt0001-nr_qtd_umidade.
          wa_retorno-nr_perc_impureza = wa_zsdt0001-nr_perc_impureza.
          wa_retorno-nr_qtd_impureza  = wa_zsdt0001-nr_qtd_impureza.
          wa_retorno-nr_perc_avaria   = wa_zsdt0001-nr_perc_avaria.
          wa_retorno-nr_qtd_avaria    = wa_zsdt0001-nr_qtd_avaria.
          wa_retorno-nr_perc_ardido   = wa_zsdt0001-nr_perc_ardido.
          wa_retorno-nr_qtd_ardido    = wa_zsdt0001-nr_qtd_ardido.
          wa_retorno-nr_perc_quebra   = wa_zsdt0001-nr_perc_quebra.
          wa_retorno-nr_qtd_quebra    = wa_zsdt0001-nr_qtd_quebra.
          wa_retorno-nr_perc_esverd   = wa_zsdt0001-nr_perc_esverd.
          wa_retorno-nr_qtd_esverd    = wa_zsdt0001-nr_qtd_esverd.
        ENDIF.
      ENDIF.

      READ TABLE it_l2 WITH KEY docnum = wa_zlest0039-docnum TRANSPORTING NO FIELDS.
      IF sy-subrc IS INITIAL.
        LOOP AT it_l2 INTO DATA(wa_l2) WHERE docnum EQ wa_zlest0039-docnum.
          SELECT SINGLE name1 INTO wa_retorno-emit_rsocial FROM lfa1 WHERE stcd1 EQ wa_l2-cnpjferro.
          wa_retorno-emit_cnpj       = wa_l2-cnpjferro.
          wa_retorno-dcl             = wa_l2-dcl.
          wa_retorno-seriedcl        = wa_l2-seriedcl.
          wa_retorno-cnpjferro       = wa_l2-cnpjferro.
          wa_retorno-idvagao         = wa_l2-idvagao.
          wa_retorno-qt_saida        = wa_l2-peso_saida_vagao.
          wa_retorno-dt_saida        = wa_l2-data_saida_vagao.
          wa_retorno-peso_saida      = wa_l2-peso_saida_nota.
          APPEND wa_retorno TO lc_retorno.
        ENDLOOP.
      ELSE.
        APPEND wa_retorno TO lc_retorno.
      ENDIF.

    ENDLOOP.

    e_registros = lc_retorno.
    e_pesquisou = abap_true.

  ENDMETHOD.
ENDCLASS.
