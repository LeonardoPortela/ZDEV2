*&---------------------------------------------------------------------*
*& Report  ZSDR019
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT zsdr019.


TABLES: zauth_webservice, zauth_ws_0001,kna1,zsdt0197.

TYPES: BEGIN OF ty_categoia_sementes,
         id   TYPE i,
         nome TYPE char100,
       END OF ty_categoia_sementes.

TYPES: BEGIN OF ty_especie,
         id             TYPE i,
         nomecientifico TYPE char255,
         nomecomum      TYPE char50,
       END OF ty_especie.

TYPES: BEGIN OF ty_cultivar,
         id   TYPE i,
         nome TYPE char100,
       END OF ty_cultivar.


TYPES: BEGIN OF ty_praga,
         id   TYPE i,
         nome TYPE char100,
       END OF ty_praga.

TYPES: BEGIN OF ty_pessoal_autorizado,
         cpfcnpj   TYPE char14,
         nome      TYPE char100,
         municipio TYPE char100,
         tipo      TYPE char50,
       END OF  ty_pessoal_autorizado.

TYPES: BEGIN OF ty_embalagem,
         id   TYPE i,
         nome TYPE char100,
       END OF ty_embalagem.

*--Ajuste BG Atualização SISDEV US 122737

TYPES: BEGIN OF ty_unidade_medida,
         id    TYPE i,
         nome  TYPE char100,
         sigla TYPE char20,
       END OF ty_unidade_medida.

DATA:
  BEGIN OF ty_tipoaplicacao,
    id            TYPE i,
    nome          TYPE char100,
    unidademedida TYPE TABLE OF ty_unidade_medida,
  END OF ty_tipoaplicacao.

*--Ajuste BG Atualização SISDEV US 122737



TYPES: BEGIN OF ty_produto,
         id            TYPE i,
         nome          TYPE char100,
         codmapa       TYPE char10,
         fabricante    TYPE char100,
         embalagem     TYPE char100,
         tipoembalagem TYPE char100,
         volume        TYPE p DECIMALS 2,
         unidade       TYPE char50,
       END OF ty_produto.

TYPES: BEGIN OF ty_ure,
         id                       TYPE i,
         idpessoa                 TYPE i,
         cnpj                     TYPE char14,
         nome                     TYPE char100,
         apelido                  TYPE char100,
         municipio                TYPE char100,
         autorizado               TYPE char1,
         qtdrecebimentoweb        TYPE zqtde_t,
         qtdrecebimentowebservice TYPE zqtde_t,
         qtddestinacaoweb         TYPE zqtde_t,
         qtddestinacaowebservice  TYPE zqtde_t,
       END OF   ty_ure.

TYPES: BEGIN OF ty_rtc,
         cpf    TYPE char30,
         nome   TYPE char100,
         email  TYPE char100,
         status TYPE char01,
       END OF   ty_rtc.

TYPES: BEGIN OF ty_propriedade,
         id        TYPE i,
         codigo    TYPE char20,
         nome      TYPE char100,
         municipio TYPE char50,
         viaacesso TYPE char255,
         latitude  TYPE zsdt0207-latitude,
         longitude TYPE zsdt0207-longitude,
       END OF ty_propriedade.

TYPES: BEGIN OF ty_produtor,
         id        TYPE i,
         cnpj      TYPE char14,
         nome      TYPE char100,
         municipio TYPE char50,
       END OF ty_produtor.


TYPES: BEGIN OF ty_zauth_webservice,
         service   TYPE zauth_webservice-service,
         url       TYPE zauth_webservice-url,
         sequencia TYPE i,
       END OF ty_zauth_webservice.

TYPES: BEGIN OF ty_kna1,
         kunnr TYPE kna1-kunnr,
         name1 TYPE kna1-name1,
       END OF ty_kna1.


TYPES: BEGIN OF ty_saldo_revenda,
         id                    TYPE i,
         cnpjrevenda           TYPE char14,
         nomerevenda           TYPE char100,
         municipiorevenda      TYPE char20,
         cnpjlocalestoque      TYPE char14,
         nomelocalestoque      TYPE char100,
         municipiolocalestoque TYPE char20,
         idprodutoembalagem    TYPE i,
         nomeproduto           TYPE char100,
         embalagem             TYPE char100,
         tamanhoembalagem      TYPE i,
         lote                  TYPE char10,
         saldo                 TYPE zsdt0207-latitude,
         unidademedida         TYPE char10,
         tipoproduto           TYPE char100,
         especie               TYPE char100,
         idcultivar            TYPE i,
         cultivar              TYPE char100,
         renasemrevenda        TYPE char50,
         renasemlocalestoque   TYPE char50,
         categoriasementes     TYPE char50,
       END OF  ty_saldo_revenda.

TYPES: BEGIN OF ty_saida_exec,
         msg(255),
       END OF ty_saida_exec.

TYPES: BEGIN OF ty_estrutura.
         INCLUDE TYPE slis_fieldcat_main.
         INCLUDE TYPE slis_fieldcat_alv_spec.
TYPES: END OF ty_estrutura.

DATA: it_categoria_sementes TYPE TABLE OF ty_categoia_sementes,
      it_especie            TYPE TABLE OF ty_especie,
      it_cultivar           TYPE TABLE OF ty_cultivar,
      it_praga              TYPE TABLE OF ty_praga,
      it_pessoal_autorizado TYPE TABLE OF ty_pessoal_autorizado,
      it_embalagem          TYPE TABLE OF ty_embalagem,
      it_tp_aplicacao       LIKE TABLE OF ty_tipoaplicacao, "ty_tp_aplicacao, ajuste  - BG US 122737
      it_unidade_medida     TYPE TABLE OF ty_unidade_medida,
      it_produto            TYPE TABLE OF ty_produto,
      it_propriedade        TYPE TABLE OF ty_propriedade,
      it_produtor           TYPE TABLE OF ty_produtor,
      it_produtor_aux       TYPE TABLE OF ty_produtor,
      wa_produtor_aux       TYPE ty_produtor,
      it_ure                TYPE TABLE OF ty_ure,
      it_rtc                TYPE TABLE OF ty_rtc,
      it_saldo_revenda      TYPE TABLE OF ty_saldo_revenda.


DATA: wa_zsdt0197  TYPE zsdt0197, "TABELA DE ESPECIE
      wa_zsdt0198  TYPE zsdt0198, "TABELA CULTIVAR
      wa_zsdt0199  TYPE zsdt0199, "TABELA CATEGORIA SEMENTES
      wa_zsdt0200  TYPE zsdt0200, "TABELA TIPO DE EMBALAGEM
      wa_zsdt0201  TYPE zsdt0201, "TABELA PRODUTO AUTORIZADO
      wa_zsdt0202  TYPE zsdt0202, "TABELA TIPO DE APLICAÇÃO
      wa_zsdt0203  TYPE zsdt0203, "TABELA PRAGA
      wa_zsdt0204  TYPE zsdt0204, "TABELA UNIDADE DE MEDIDA
      wa_zsdt0205  TYPE zsdt0205, "TABELA PESSOA AUTORIZADA ADQUIRIR AGROTOXICO
      wa_zsdt0206  TYPE zsdt0206, "TABELA PRODUTOR
      wa_zsdt0207  TYPE zsdt0207, "TABELA PROPRIEDADE
      wa_zsdt0208  TYPE zsdt0208, "TABELA URE
      wa_zsdt0259  TYPE zsdt0259, "RTC - Responsável Técnico Comercial - SIAGRI
      wa_zsdt0209  TYPE zsdt0209, "TABELA SALDO REVENDA
      wa_estrutura TYPE ty_estrutura.


DATA: it_kna1 TYPE TABLE OF kna1,
      wa_kna1 TYPE kna1.


DATA: it_cliente  TYPE TABLE OF ty_kna1,
      wa_cliente  TYPE kna1,
      it_zsdt0217 TYPE TABLE OF zsdt0217,
      wa_zsdt0217 TYPE zsdt0217.

DATA: it_zauth_webservice TYPE TABLE OF ty_zauth_webservice,
      wa_zauth_webservice TYPE ty_zauth_webservice,
      it_zauth_ws_0001    TYPE TABLE OF zauth_ws_0001,
      wa_zauth_ws_0001    TYPE zauth_ws_0001.


DATA: ob_web_service TYPE REF TO zcl_webservice.
DATA: e_reason     TYPE string,
      json_retorno TYPE string,
      e_http       TYPE REF TO  if_http_client,
      e_xml        TYPE string.

DATA purl TYPE zauth_webservice-url.
DATA vprodutor.
DATA seq TYPE zsdt0209-id_saldo_rev.
DATA vcheck.

CREATE OBJECT ob_web_service.

DATA: t_tabela TYPE  zrsdsselopts.
DATA vcpnj TYPE zauth_ws_0001-username.
"DATA: VSERVICE TYPE ZAUTH_WS_0001-SERVICE.

DATA: r_service TYPE RANGE OF zauth_ws_0001-service,
      w_service LIKE LINE OF r_service.

DATA: tl_set        TYPE TABLE OF rgsb4      WITH HEADER LINE,
      w_ktokd       TYPE rgsb4,
      tl_return     TYPE TABLE OF bapiret2   WITH HEADER LINE,
      estrutura     TYPE TABLE OF ty_estrutura,
      tl_saida_exec TYPE TABLE OF ty_saida_exec WITH HEADER LINE.

RANGES: r_ktokd       FOR kna1-ktokd.

"CONSTANTS: "vure_especie  TYPE zauth_webservice-url VALUE 'https://vegetal.indea.mt.gov.br/SISDEV/ws/especie/list',
"vure_produtor TYPE zauth_webservice-url VALUE 'https://vegetal.indea.mt.gov.br/SISDEV/ws/produtor/cpfCnpj/'.


SELECT-OPTIONS: p_tabela FOR zauth_ws_0001-service  NO-DISPLAY.
SELECT-OPTIONS: s_kunnr FOR kna1-kunnr  NO-DISPLAY.
SELECT-OPTIONS: s_espec FOR zsdt0197-id_especie NO-DISPLAY. " Rubenilson - 07.04.25 #168932
PARAMETERS    : p_nopop TYPE char01 NO-DISPLAY.             "*-US191683-25.09.2025-#191683-JT-inicio

INITIALIZATION.


START-OF-SELECTION.

  IF p_tabela IS NOT INITIAL.

    REFRESH r_service.
    CLEAR  w_service.

    LOOP AT p_tabela.
      w_service-sign    = 'I'.
      w_service-option  = p_tabela-option.
      w_service-low     = p_tabela-low.
      w_service-high    = p_tabela-high.
      APPEND w_service TO r_service.
      CLEAR w_service.
    ENDLOOP.
    vcheck = 'X'.
  ELSE.
    w_service-sign    = 'I'.
    w_service-option  = 'CP'.
    w_service-low     = 'INDEA_CONSULTA_*'.
    APPEND w_service TO r_service.
    CLEAR w_service.
    vcheck = ''.
  ENDIF.

  CALL FUNCTION 'G_SET_GET_ALL_VALUES'
    EXPORTING
      class           = '0000'
      setnr           = 'ZSDR019_KTOKD'
      no_descriptions = space
      no_rw_info      = space
    TABLES
      set_values      = tl_set
    EXCEPTIONS
      set_not_found   = 1
      OTHERS          = 2.

  LOOP AT tl_set INTO w_ktokd.
    r_ktokd-sign   = 'I'.
    r_ktokd-option = 'EQ'.
    r_ktokd-low    = w_ktokd-from.
    APPEND r_ktokd.
  ENDLOOP.


  SELECT  *
    FROM kna1 INTO TABLE it_kna1
   WHERE kunnr IN s_kunnr.
*     AND ktokd IN ( 'ZCFU', 'ZCIC', 'ZCNF', 'ZCNJ', 'ZCPF', 'ZCPJ' ). "02.09.25 - Projeto Renova Insumos - fim

  SELECT  service
          url
    FROM zauth_webservice INTO TABLE it_zauth_webservice
  WHERE service IN r_service.

  LOOP AT it_zauth_webservice INTO wa_zauth_webservice.
    IF wa_zauth_webservice-service     EQ 'INDEA_CONSULTA_CAT_SEMENTES'.
      wa_zauth_webservice-sequencia = 1.
    ELSEIF wa_zauth_webservice-service EQ 'INDEA_CONSULTA_ESPECIE'.
      wa_zauth_webservice-sequencia = 2.
    ELSEIF wa_zauth_webservice-service EQ 'INDEA_CONSULTA_PESSOA_AUTORIAZADA'.
      wa_zauth_webservice-sequencia = 3.
    ELSEIF wa_zauth_webservice-service EQ 'INDEA_CONSULTA_PRAGA'.
      wa_zauth_webservice-sequencia = 4.
    ELSEIF wa_zauth_webservice-service EQ 'INDEA_CONSULTA_PRODUTO'.
      wa_zauth_webservice-sequencia = 5.
    ELSEIF wa_zauth_webservice-service EQ 'INDEA_CONSULTA_SITUACAO_EMBALAGEM'.
      wa_zauth_webservice-sequencia = 6.
    ELSEIF wa_zauth_webservice-service EQ 'INDEA_CONSULTA_TIPO_APLICACAO'.
      wa_zauth_webservice-sequencia = 7.
    ELSEIF wa_zauth_webservice-service EQ 'INDEA_CONSULTA_UNIDADE_MEDIDA'.
      wa_zauth_webservice-sequencia = 8.
    ELSEIF wa_zauth_webservice-service EQ 'INDEA_CONSULTA_CULTIVAR'.
      wa_zauth_webservice-sequencia = 9.
    ELSEIF wa_zauth_webservice-service EQ 'INDEA_CONSULTA_PRODUTOR'.
      wa_zauth_webservice-sequencia = 10.
    ELSEIF wa_zauth_webservice-service EQ 'INDEA_CONSULTA_PROPRIEDADE'.
      wa_zauth_webservice-sequencia = 11.
    ELSEIF wa_zauth_webservice-service EQ 'INDEA_CONSULTA_URE'.
      wa_zauth_webservice-sequencia = 12.
    ELSEIF wa_zauth_webservice-service EQ 'INDEA_CONSULTA_SALDO_REVENDA'.
      wa_zauth_webservice-sequencia = 13.
    ENDIF.
    MODIFY it_zauth_webservice FROM wa_zauth_webservice INDEX sy-tabix.
    CLEAR wa_zauth_webservice.
  ENDLOOP.


  SELECT *
    FROM zauth_ws_0001 INTO TABLE it_zauth_ws_0001
    FOR ALL ENTRIES IN it_zauth_webservice
   WHERE service EQ it_zauth_webservice-service.

  FREE tl_return.

  SORT it_zauth_webservice BY sequencia.

  LOOP AT it_zauth_webservice INTO wa_zauth_webservice.

    READ TABLE it_zauth_ws_0001 INTO wa_zauth_ws_0001 WITH KEY service = wa_zauth_webservice-service.
    IF sy-subrc = 0.

      CASE  wa_zauth_webservice-service.
        WHEN 'INDEA_CONSULTA_CAT_SEMENTES'.

          PERFORM z_consulta_indea_lista USING  wa_zauth_webservice-url
                                                wa_zauth_ws_0001-username
                                                wa_zauth_ws_0001-password
                                        CHANGING it_categoria_sementes.

          LOOP AT it_categoria_sementes INTO DATA(wa_cat_sementes).

            wa_zsdt0199-mandt            = sy-mandt.
            wa_zsdt0199-id_cat_sementes  = wa_cat_sementes-id.
            wa_zsdt0199-nome             = wa_cat_sementes-nome.
            wa_zsdt0199-data_atual       = sy-datum.
            wa_zsdt0199-hora_atual       = sy-uzeit.
            wa_zsdt0199-usname           = sy-uname.

            MODIFY zsdt0199 FROM wa_zsdt0199.
            COMMIT WORK.

            CLEAR: wa_cat_sementes, wa_zsdt0199.
          ENDLOOP.

          APPEND VALUE #( message = 'Dados importado com sucesso!' type = 'S' ) TO tl_return.

        WHEN 'INDEA_CONSULTA_ESPECIE'.

          PERFORM z_consulta_indea_lista USING  wa_zauth_webservice-url
                                                wa_zauth_ws_0001-username
                                                wa_zauth_ws_0001-password
                                     CHANGING it_especie.

*** Inicio - Rubenilson Pereira - 07.04.25 #168932
          IF s_espec[] IS NOT INITIAL.
            DELETE it_especie WHERE id NOT IN s_espec[].
          ENDIF.
*** Fim - Rubenilson Pereira - 07.04.25 #168932

          LOOP AT it_especie INTO DATA(wa_especie).

            wa_zsdt0197-mandt            = sy-mandt.
            wa_zsdt0197-id_especie       = wa_especie-id.
            wa_zsdt0197-nomecientifico   = wa_especie-nomecientifico.
            wa_zsdt0197-nomecomum        = wa_especie-nomecomum.
            wa_zsdt0197-data_atual       = sy-datum.
            wa_zsdt0197-hora_atual       = sy-uzeit.
            wa_zsdt0197-usname           = sy-uname.

            MODIFY zsdt0197 FROM wa_zsdt0197.
            COMMIT WORK.

            CLEAR: wa_especie, wa_zsdt0197.
          ENDLOOP.

          APPEND VALUE #( message = 'Dados importado com sucesso!' type = 'S' ) TO tl_return.


        WHEN 'INDEA_CONSULTA_PESSOA_AUTORIAZADA'.

          LOOP AT it_kna1 INTO wa_kna1.

            IF wa_kna1-ktokd IN r_ktokd[].
              CLEAR purl.

              IF wa_kna1-stkzn IS NOT INITIAL.
                purl = |{ wa_zauth_webservice-url }{ wa_kna1-stcd2 }|.
              ELSE.
                purl = |{ wa_zauth_webservice-url }{ wa_kna1-stcd1 }|.
              ENDIF.

              vprodutor = abap_true.

              PERFORM z_consulta_indea_lista USING  purl
                                                    wa_zauth_ws_0001-username
                                                    wa_zauth_ws_0001-password
                                         CHANGING it_pessoal_autorizado.

              LOOP AT it_pessoal_autorizado INTO DATA(wa_pessoa_aut).
                wa_zsdt0205-mandt       = sy-mandt.
                wa_zsdt0205-cpfcnpj     = wa_pessoa_aut-cpfcnpj.
                wa_zsdt0205-nome        = wa_pessoa_aut-nome.
                wa_zsdt0205-municipio   = wa_pessoa_aut-municipio.
                wa_zsdt0205-tipo        = wa_pessoa_aut-tipo.
                wa_zsdt0205-data_atual  = sy-datum.
                wa_zsdt0205-hora_atual  = sy-uzeit.
                wa_zsdt0205-usname      = sy-uname.

                MODIFY zsdt0205 FROM wa_zsdt0205.
                COMMIT WORK.

                CLEAR: wa_pessoa_aut, wa_zsdt0205.
              ENDLOOP.

              APPEND VALUE #( message = |Dados do Cliente { wa_kna1-kunnr } importado com sucesso!|  type = 'S' ) TO tl_return.
            ELSE.
              APPEND VALUE #( message = | "Grupo de contas { wa_kna1-ktokd } do Cliente { wa_kna1-kunnr } não está parametrizado para baixar dados do SISDEV"| type = 'E' ) TO tl_return.
            ENDIF.

            CLEAR: wa_kna1.
            REFRESH it_pessoal_autorizado.
          ENDLOOP.


        WHEN 'INDEA_CONSULTA_PRAGA'.
          PERFORM z_consulta_indea_lista USING  wa_zauth_webservice-url
                                                wa_zauth_ws_0001-username
                                                wa_zauth_ws_0001-password
                                     CHANGING it_praga.

          LOOP AT it_praga INTO DATA(wa_praga).
            wa_zsdt0203-mandt       = sy-mandt.
            wa_zsdt0203-id_praga    = wa_praga-id.
            wa_zsdt0203-nome        = wa_praga-nome.
            wa_zsdt0203-data_atual  = sy-datum.
            wa_zsdt0203-hora_atual  = sy-uzeit.
            wa_zsdt0203-usname      = sy-uname.

            MODIFY zsdt0203 FROM wa_zsdt0203.
            COMMIT WORK.

            CLEAR: wa_zsdt0203, wa_praga.
          ENDLOOP.

          APPEND VALUE #( message = 'Dados importado com sucesso!' type = 'S' ) TO tl_return.

        WHEN 'INDEA_CONSULTA_PRODUTO'.

          IF vcheck = 'X'.

            SELECT *
              FROM zsdt0217 INTO TABLE it_zsdt0217
             WHERE busca_indea = 'X'
              AND  validade > sy-datum.

          ELSE.

            SELECT *
              FROM zsdt0217 INTO TABLE it_zsdt0217
             WHERE  validade > sy-datum.

          ENDIF.

          CHECK it_zsdt0217 IS NOT INITIAL.

          LOOP AT it_zsdt0217 INTO wa_zsdt0217.

            CLEAR purl.
            purl = |{ wa_zauth_webservice-url }{ wa_zsdt0217-nr_reg }|.

            PERFORM z_consulta_indea_lista USING  purl
                                                  wa_zauth_ws_0001-username
                                                  wa_zauth_ws_0001-password
                                       CHANGING it_produto.

            LOOP AT it_produto INTO DATA(wa_produto).

              wa_zsdt0201-mandt           = sy-mandt.
              wa_zsdt0201-id_produto      = wa_produto-id.
              wa_zsdt0201-nome            = wa_produto-nome.
              wa_zsdt0201-codmapa         = wa_produto-codmapa.
              wa_zsdt0201-fabricante      = wa_produto-fabricante.
              wa_zsdt0201-embalagem       = wa_produto-embalagem.
              wa_zsdt0201-tipoembalagem   = wa_produto-tipoembalagem.
              wa_zsdt0201-volume          = wa_produto-volume.
              wa_zsdt0201-unidade         = wa_produto-unidade.
              wa_zsdt0201-data_atual      = sy-datum.
              wa_zsdt0201-hora_atual      = sy-uzeit.
              wa_zsdt0201-usname          = sy-uname.

              MODIFY zsdt0201 FROM wa_zsdt0201.
              COMMIT WORK.

              CLEAR: wa_produto, wa_zsdt0201.
            ENDLOOP.

            UPDATE zsdt0217 SET busca_indea =  ''
             WHERE nr_cad  = wa_zsdt0217-nr_cad
             AND   nr_reg  = wa_zsdt0217-nr_reg.

            CLEAR: wa_zsdt0217.
          ENDLOOP.

          APPEND VALUE #( message = 'Dados importado com sucesso!' type = 'S' ) TO tl_return.

        WHEN 'INDEA_CONSULTA_SITUACAO_EMBALAGEM'.
          PERFORM z_consulta_indea_lista USING  wa_zauth_webservice-url
                                                wa_zauth_ws_0001-username
                                                wa_zauth_ws_0001-password
                                     CHANGING it_embalagem.

          LOOP AT it_embalagem INTO DATA(wa_embalagem).

            wa_zsdt0200-mandt              = sy-mandt.
            wa_zsdt0200-id_st_embalagem    = wa_embalagem-id.
            wa_zsdt0200-nome               = wa_embalagem-nome.
            wa_zsdt0200-data_atual         = sy-datum.
            wa_zsdt0200-hora_atual         = sy-uzeit.
            wa_zsdt0200-usname             = sy-uname.

            MODIFY zsdt0200 FROM wa_zsdt0200.
            COMMIT WORK.

            CLEAR: wa_zsdt0200, wa_embalagem.
          ENDLOOP.

          APPEND VALUE #( message = 'Dados importado com sucesso!' type = 'S' ) TO tl_return.

        WHEN 'INDEA_CONSULTA_TIPO_APLICACAO'.

*--Ajuste BG Atualização SISDEV US 122737   inicio2
          PERFORM z_consulta_indea_lista USING  wa_zauth_webservice-url
                                                wa_zauth_ws_0001-username
                                                wa_zauth_ws_0001-password
                                     CHANGING it_tp_aplicacao.

          LOOP AT it_tp_aplicacao INTO DATA(wa_tp_aplicacao).

            LOOP AT wa_tp_aplicacao-unidademedida INTO DATA(wa_unid_medida).

              wa_zsdt0202-mandt             = sy-mandt.
              wa_zsdt0202-id_tp_aplicacao   = wa_tp_aplicacao-id.
              wa_zsdt0202-nome              = wa_tp_aplicacao-nome.
              wa_zsdt0202-data_atual        = sy-datum.
              wa_zsdt0202-hora_atual        = sy-uzeit.
              wa_zsdt0202-usname            = sy-uname.
              wa_zsdt0202-id_unid_medida   = wa_unid_medida-id.
              wa_zsdt0202-nome_unid_medida  = wa_unid_medida-nome.
              wa_zsdt0202-sigla_unid_medida = wa_unid_medida-sigla.
              MODIFY zsdt0202 FROM wa_zsdt0202.
              COMMIT WORK.
              CLEAR: wa_zsdt0202, wa_tp_aplicacao.
            ENDLOOP.

            CLEAR: wa_zsdt0202, wa_tp_aplicacao.
          ENDLOOP.
*--Ajuste BG Atualização SISDEV US 122737 fim2

          APPEND VALUE #( message = 'Dados importado com sucesso!' type = 'S' ) TO tl_return.

        WHEN 'INDEA_CONSULTA_UNIDADE_MEDIDA'.
          PERFORM z_consulta_indea_lista USING  wa_zauth_webservice-url
                                                wa_zauth_ws_0001-username
                                                wa_zauth_ws_0001-password
                                     CHANGING it_unidade_medida.

          LOOP AT it_unidade_medida INTO DATA(wa_unidade).

            wa_zsdt0204-mandt            = sy-mandt.
            wa_zsdt0204-id_unid_medida   = wa_unidade-id.
            wa_zsdt0204-nome             = wa_unidade-nome.
            wa_zsdt0204-sigla            = wa_unidade-sigla.
            wa_zsdt0204-data_atual       = sy-datum.
            wa_zsdt0204-hora_atual       = sy-uzeit.
            wa_zsdt0204-usname           = sy-uname.

            MODIFY zsdt0204 FROM wa_zsdt0204.
            COMMIT WORK.

            CLEAR: wa_zsdt0204, wa_unidade.
          ENDLOOP.

          APPEND VALUE #( message = 'Dados importado com sucesso!' type = 'S' ) TO tl_return.

        WHEN 'INDEA_CONSULTA_CULTIVAR'.

          SELECT SINGLE *
            FROM zauth_webservice INTO @DATA(lwa_service_especie)
           WHERE service = 'INDEA_CONSULTA_ESPECIE'.

          IF it_especie IS INITIAL AND lwa_service_especie-url IS NOT INITIAL.

            PERFORM z_consulta_indea_lista USING  lwa_service_especie-url
                                                  wa_zauth_ws_0001-username
                                                  wa_zauth_ws_0001-password
                                       CHANGING it_especie.

*** Inicio - Rubenilson Pereira - 07.04.25 #168932
            IF s_espec[] IS NOT INITIAL.
              DELETE it_especie WHERE id NOT IN s_espec[].
            ENDIF.
*** Fim - Rubenilson Pereira - 07.04.25 #168932

            LOOP AT it_especie INTO wa_especie.

              wa_zsdt0197-mandt            = sy-mandt.
              wa_zsdt0197-id_especie       = wa_especie-id.
              wa_zsdt0197-nomecientifico   = wa_especie-nomecientifico.
              wa_zsdt0197-nomecomum        = wa_especie-nomecomum.
              wa_zsdt0197-data_atual       = sy-datum.
              wa_zsdt0197-hora_atual       = sy-uzeit.
              wa_zsdt0197-usname           = sy-uname.

              MODIFY zsdt0197 FROM wa_zsdt0197.
              COMMIT WORK.

              CLEAR: wa_especie, wa_zsdt0197.
            ENDLOOP.

          ENDIF.

          LOOP AT it_especie INTO wa_especie.
            CLEAR purl.
            purl = |{ wa_zauth_webservice-url }{ wa_especie-id }|.

            PERFORM z_consulta_indea_lista USING  purl
                                                  wa_zauth_ws_0001-username
                                                  wa_zauth_ws_0001-password
                                            CHANGING it_cultivar.

            LOOP AT it_cultivar INTO DATA(wa_cultivar).


              wa_zsdt0198-mandt         =  sy-mandt.
              wa_zsdt0198-id_cultivar   = wa_cultivar-id.
              wa_zsdt0198-id_especie    = wa_especie-id.
              wa_zsdt0198-nome          = wa_cultivar-nome.
              wa_zsdt0198-data_atual    = sy-datum.
              wa_zsdt0198-hora_atual    = sy-uzeit.
              wa_zsdt0198-usname        = sy-uname.

              MODIFY zsdt0198 FROM wa_zsdt0198.
              COMMIT WORK.

              CLEAR: wa_zsdt0198, wa_cultivar.
            ENDLOOP.

            CLEAR wa_especie.
            REFRESH it_cultivar.

          ENDLOOP.

          APPEND VALUE #( message = 'Dados importado com sucesso!' type = 'S' ) TO tl_return.

        WHEN 'INDEA_CONSULTA_PRODUTOR'.

          LOOP AT it_kna1 INTO wa_kna1.
            CLEAR purl.

            IF wa_kna1-ktokd IN r_ktokd[].
              IF wa_kna1-stkzn IS NOT INITIAL.
                purl = |{ wa_zauth_webservice-url }{ wa_kna1-stcd2 }|.
              ELSE.
                purl = |{ wa_zauth_webservice-url }{ wa_kna1-stcd1 }|.
              ENDIF.

              vprodutor = abap_true.

              PERFORM z_consulta_indea_lista USING  purl
                                                    wa_zauth_ws_0001-username
                                                    wa_zauth_ws_0001-password
                                              CHANGING it_produtor.

              LOOP AT it_produtor INTO DATA(wa_produtor).

                wa_zsdt0206-mandt         = sy-mandt.
                wa_zsdt0206-id_produtor   = wa_produtor-id.
                wa_zsdt0206-kunnr         = wa_kna1-kunnr.
                wa_zsdt0206-cnpj          = wa_produtor-cnpj.
                wa_zsdt0206-nome          = wa_produtor-nome.
                wa_zsdt0206-municipio     = wa_produtor-municipio.
                wa_zsdt0206-data_atual    = sy-datum.
                wa_zsdt0206-hora_atual    = sy-uzeit.
                wa_zsdt0206-usname        = sy-uname.

                APPEND wa_produtor TO it_produtor_aux.

                MODIFY zsdt0206 FROM wa_zsdt0206.
                COMMIT WORK.

                CLEAR: wa_zsdt0206, wa_produtor.
              ENDLOOP.

              APPEND VALUE #( message = |Dados do Cliente { wa_kna1-kunnr } importado com sucesso!|  type = 'S' ) TO tl_return.
            ELSE.
              APPEND VALUE #( message = | "Grupo de contas { wa_kna1-ktokd } do Cliente { wa_kna1-kunnr } não está parametrizado para baixar dados do SISDEV"| type = 'E' ) TO tl_return.
            ENDIF.

            CLEAR: wa_kna1.
            REFRESH it_produtor.
          ENDLOOP.


        WHEN 'INDEA_CONSULTA_PROPRIEDADE'.

          SELECT SINGLE *
            FROM zauth_webservice INTO @DATA(lwa_service_produtor)
           WHERE service = 'INDEA_CONSULTA_PRODUTOR'.

          IF ( it_produtor_aux IS INITIAL ) AND ( lwa_service_produtor-url IS NOT INITIAL ).

            LOOP AT it_kna1 INTO wa_kna1.
              CLEAR purl.


              IF wa_kna1-ktokd IN r_ktokd[].
                IF wa_kna1-stkzn IS NOT INITIAL.
                  purl = |{ lwa_service_produtor-url }{ wa_kna1-stcd2 }|.
                ELSE.
                  purl = |{ lwa_service_produtor-url }{ wa_kna1-stcd1 }|.
                ENDIF.

                vprodutor = abap_true.

                PERFORM z_consulta_indea_lista USING  purl
                                                      wa_zauth_ws_0001-username
                                                      wa_zauth_ws_0001-password
                                                CHANGING it_produtor.

                LOOP AT it_produtor INTO wa_produtor.

                  wa_zsdt0206-mandt         = sy-mandt.
                  wa_zsdt0206-id_produtor   = wa_produtor-id.
                  wa_zsdt0206-kunnr         = wa_kna1-kunnr.
                  wa_zsdt0206-cnpj          = wa_produtor-cnpj.
                  wa_zsdt0206-nome          = wa_produtor-nome.
                  wa_zsdt0206-municipio     = wa_produtor-municipio.
                  wa_zsdt0206-data_atual    = sy-datum.
                  wa_zsdt0206-hora_atual    = sy-uzeit.
                  wa_zsdt0206-usname        = sy-uname.

                  APPEND wa_produtor TO it_produtor_aux.

                  MODIFY zsdt0206 FROM wa_zsdt0206.
                  COMMIT WORK.

                  CLEAR: wa_zsdt0206, wa_produtor.
                ENDLOOP.

                APPEND VALUE #( message = |Dados do Cliente { wa_kna1-kunnr } importado com sucesso!|  type = 'S' ) TO tl_return.
              ELSE.
                APPEND VALUE #( message = | "Grupo de contas { wa_kna1-ktokd } do Cliente { wa_kna1-kunnr } não está parametrizado para baixar dados do SISDEV"| type = 'E' ) TO tl_return.
              ENDIF.

              CLEAR: wa_kna1.
              REFRESH it_produtor.

            ENDLOOP.

          ENDIF.

          SORT it_produtor_aux BY id.

          DATA(lt_produtor) = it_produtor_aux.
          SORT lt_produtor BY id.
          DELETE ADJACENT DUPLICATES FROM lt_produtor COMPARING id.
          IF lt_produtor IS NOT INITIAL.
            SELECT *
              FROM zsdt0207
              INTO TABLE @DATA(lt_zsdt0207)
              FOR ALL ENTRIES IN @lt_produtor
              WHERE id_produtor = @lt_produtor-id.
            IF sy-subrc IS INITIAL.
              SORT lt_zsdt0207 BY id_propriedade.
            ENDIF.
          ENDIF.

          LOOP AT it_produtor_aux INTO wa_produtor_aux.
            CLEAR purl.
            purl = |{ wa_zauth_webservice-url }{ wa_produtor_aux-cnpj }|.

            PERFORM z_consulta_indea_lista USING  purl
                                                  wa_zauth_ws_0001-username
                                                  wa_zauth_ws_0001-password
                                            CHANGING it_propriedade.

            LOOP AT it_propriedade INTO DATA(wa_propriedade).

              READ TABLE lt_zsdt0207 TRANSPORTING NO FIELDS
              WITH KEY id_propriedade = wa_propriedade-id
              BINARY SEARCH.
              IF sy-subrc IS INITIAL.
                DELETE lt_zsdt0207 INDEX sy-tabix.
              ENDIF.

              wa_zsdt0207-mandt            = sy-mandt.
              wa_zsdt0207-id_propriedade   = wa_propriedade-id.
              wa_zsdt0207-id_produtor      = wa_produtor_aux-id.
              wa_zsdt0207-codigo           = wa_propriedade-codigo.
              wa_zsdt0207-municipio        = wa_propriedade-municipio.
              wa_zsdt0207-nome             = wa_propriedade-nome.
              wa_zsdt0207-via_acesso       = wa_propriedade-viaacesso.
              wa_zsdt0207-latitude         = wa_propriedade-latitude.
              wa_zsdt0207-longitude        = wa_propriedade-longitude.
              wa_zsdt0207-data_atual       = sy-datum.
              wa_zsdt0207-hora_atual       = sy-uzeit.
              wa_zsdt0207-usname           = sy-uname.

              MODIFY zsdt0207 FROM wa_zsdt0207.
              COMMIT WORK.

              CLEAR: wa_zsdt0207, wa_propriedade.
            ENDLOOP.

            APPEND VALUE #( message = |Propriedade do Cliente { wa_kna1-kunnr } importadas com sucesso!|  type = 'S' ) TO tl_return.

            CLEAR wa_produtor_aux.
            REFRESH it_propriedade.
          ENDLOOP.

          LOOP AT lt_zsdt0207 ASSIGNING FIELD-SYMBOL(<fs_zsdt0207>).
            <fs_zsdt0207>-inativo = abap_true.
            <fs_zsdt0207>-data_i = sy-datum.
            <fs_zsdt0207>-hora_i = sy-uzeit.
            <fs_zsdt0207>-usname_i = sy-uname.
          ENDLOOP.

          MODIFY zsdt0207 FROM TABLE lt_zsdt0207.
          IF sy-subrc IS INITIAL.
            COMMIT WORK.
          ENDIF.

        WHEN 'INDEA_CONSULTA_URE'.

          SELECT *
            FROM zsdt0211 INTO TABLE @DATA(it_zsdt0211).

          CHECK it_zsdt0211 IS NOT INITIAL.

          SELECT *
            FROM lfa1 INTO TABLE @DATA(it_lfa1)
            FOR ALL ENTRIES IN @it_zsdt0211
          WHERE lifnr EQ @it_zsdt0211-lifnr.


          LOOP AT it_lfa1 INTO DATA(wa_lfa1).

            CLEAR purl.
            purl = |{ wa_zauth_webservice-url }{ wa_lfa1-stcd1 }|.

            PERFORM z_consulta_indea_lista USING  purl
                                                  wa_zauth_ws_0001-username
                                                  wa_zauth_ws_0001-password
                                            CHANGING it_ure.

            LOOP AT it_ure INTO DATA(wa_ure).

              wa_zsdt0208-id_ure                      = wa_ure-id.
              wa_zsdt0208-id_pessoa                   = wa_ure-idpessoa.
              wa_zsdt0208-cnpj                        = wa_ure-cnpj.
              wa_zsdt0208-nome                        = wa_ure-nome.
              wa_zsdt0208-apelido                     = wa_ure-apelido.
              wa_zsdt0208-municipio                   = wa_ure-municipio.
              wa_zsdt0208-autorizado                  = wa_ure-autorizado.
              wa_zsdt0208-qtdrecebimentoweb           = wa_ure-qtdrecebimentoweb.
              wa_zsdt0208-qtdrecebimentowebservice    = wa_ure-qtdrecebimentowebservice.
              wa_zsdt0208-qtddestinacaoweb            = wa_ure-qtddestinacaoweb.
              wa_zsdt0208-qtddestinacaowebservice     = wa_ure-qtddestinacaowebservice.
              wa_zsdt0208-data_atual                  = sy-datum.
              wa_zsdt0208-hora_atual                  = sy-uzeit.
              wa_zsdt0208-usname                      = sy-uname.

              MODIFY zsdt0208 FROM wa_zsdt0208.
              COMMIT WORK.
              CLEAR: wa_zsdt0208, wa_ure.
            ENDLOOP.

            CLEAR: wa_lfa1.
            REFRESH it_ure.
          ENDLOOP.

          APPEND VALUE #( message = |Dados importados com sucesso!|  type = 'S' ) TO tl_return.


        WHEN 'INDEA_CONSULTA_SALDO_REVENDA'.
          CLEAR seq.


          LOOP AT it_zauth_ws_0001 INTO DATA(wa_ws_0001)
            WHERE service EQ wa_zauth_webservice-service.

            SELECT SINGLE * FROM j_1bbranch INTO @DATA(wa_branch)
              WHERE bukrs  EQ @wa_ws_0001-bukrs
              AND   branch EQ @wa_ws_0001-branch.

            vcpnj = wa_branch-stcd1.

            PERFORM z_consulta_indea_saldo USING  wa_zauth_webservice-url
                                                  wa_ws_0001-username
                                                  wa_ws_0001-password
                                                  vcpnj
                                                  wa_ws_0001-add01
                                           CHANGING it_saldo_revenda.

            LOOP AT it_saldo_revenda INTO DATA(wa_saldo_revenda).

              seq = seq + 1.

              wa_zsdt0209-id_saldo_rev             = seq.
              wa_zsdt0209-data_atual               = sy-datum.
              wa_zsdt0209-hora_atual               = sy-uzeit.
              wa_zsdt0209-cnpjrevenda              = wa_saldo_revenda-cnpjrevenda.
              wa_zsdt0209-nomerevenda              = wa_saldo_revenda-nomerevenda.
              wa_zsdt0209-municipiorevenda         = wa_saldo_revenda-municipiorevenda.
              wa_zsdt0209-cnpjlocalestoque         = wa_saldo_revenda-cnpjlocalestoque.
              wa_zsdt0209-nomelocalestoque         = wa_saldo_revenda-nomelocalestoque.
              wa_zsdt0209-municipiolocalestoque    = wa_saldo_revenda-municipiolocalestoque.
              wa_zsdt0209-idprodutoembagalem       = wa_saldo_revenda-idprodutoembalagem.
              wa_zsdt0209-nomeproduto              = wa_saldo_revenda-nomeproduto.
              wa_zsdt0209-embalagem                = wa_saldo_revenda-embalagem.
              wa_zsdt0209-tamanhoembalagem         = wa_saldo_revenda-tamanhoembalagem.
              wa_zsdt0209-lote                     = wa_saldo_revenda-lote.
              wa_zsdt0209-saldo                    = wa_saldo_revenda-saldo.
              wa_zsdt0209-unidademedida            = wa_saldo_revenda-unidademedida.
              wa_zsdt0209-tipoproduto              = wa_saldo_revenda-tipoproduto.
              wa_zsdt0209-especie                  = wa_saldo_revenda-especie.
              wa_zsdt0209-idcultivar               = wa_saldo_revenda-idcultivar.
              wa_zsdt0209-cultiva                  = wa_saldo_revenda-cultivar.
              wa_zsdt0209-renasemrevenda           = wa_saldo_revenda-renasemrevenda.
              wa_zsdt0209-renasemlocalestoque      = wa_saldo_revenda-renasemlocalestoque.
              wa_zsdt0209-categoriasementes        = wa_saldo_revenda-categoriasementes.
              wa_zsdt0209-data_atual               = sy-datum.
              wa_zsdt0209-hora_atual               = sy-uzeit.
              wa_zsdt0209-usname                   = sy-uname.

              MODIFY zsdt0209 FROM wa_zsdt0209.
              COMMIT WORK.
              CLEAR: wa_zsdt0209, wa_saldo_revenda.
            ENDLOOP.

            REFRESH it_saldo_revenda.
            CLEAR: wa_ws_0001, vcpnj, wa_branch.
          ENDLOOP.

          APPEND VALUE #( message = |Dados importados com sucesso!|  type = 'S' ) TO tl_return.

      ENDCASE.
    ENDIF.
    CLEAR: wa_zauth_webservice, wa_zauth_ws_0001.
  ENDLOOP.

*  MESSAGE 'Dados importado com sucesso!' TYPE 'S'.
  PERFORM f_exibe_bapi.



FORM z_consulta_indea_saldo USING p_url      TYPE zauth_webservice-url
                                  p_username TYPE zauth_ws_0001-username
                                  p_password TYPE zauth_ws_0001-password
                                  p_cnpj_rev TYPE zauth_ws_0001-username
                                  p_hash_rev TYPE zauth_ws_0001-add01
                       CHANGING tabela TYPE ANY TABLE.


  DATA: v_url      TYPE string,
        v_username TYPE string,
        v_password TYPE string,
        v_cnpj_rev TYPE string,
        v_hash_rev TYPE string.

  CLEAR: v_url,v_username, v_password.

  v_url      = p_url.
  v_username = p_username.
  v_password = p_password.


  cl_http_client=>create_by_url(
  EXPORTING url   =  |{ v_url }|
  IMPORTING client = e_http
  EXCEPTIONS argument_not_found = 1
           plugin_not_active  = 2
           internal_error     = 3 ).

  CALL METHOD e_http->request->set_header_field
    EXPORTING
      name  = '~request_method'
      value = 'POST'.
  CALL METHOD e_http->request->set_header_field
    EXPORTING
      name  = '~server_protocol'
      value = 'HTTP/1.1'.

  CALL METHOD e_http->request->set_header_field
    EXPORTING
      name  = 'Content-Type'
      value = 'application/json; charset=UTF-8'.

  CALL METHOD e_http->request->set_header_field
    EXPORTING
      name  = 'Accept'
      value = 'application/json'.


  e_http->authenticate( username = v_username  password = v_password ).


  v_cnpj_rev = |"{ p_cnpj_rev }"|.
  v_hash_rev = |"{ p_hash_rev }"|.

  CONCATENATE '{"cnpjRevenda":' v_cnpj_rev ', "hashRevenda" : ' v_hash_rev '}'  INTO e_xml.

  CLEAR json_retorno.
  ob_web_service->zif_webservice~consultar(
      EXPORTING
        i_http                     = e_http
        i_xml                      = e_xml
      IMPORTING
        e_reason                   = e_reason
      RECEIVING
        e_resultado                = json_retorno
      EXCEPTIONS
        http_communication_failure = 1
        http_invalid_state         = 2
        http_processing_failed     = 3
        http_invalid_timeout       = 4
        OTHERS                     = 5  ).

  /ui2/cl_json=>deserialize( EXPORTING json = json_retorno CHANGING data = tabela ).

ENDFORM.



FORM z_consulta_indea_lista USING p_url      TYPE zauth_webservice-url
                                  p_username TYPE zauth_ws_0001-username
                                  p_password TYPE zauth_ws_0001-password
                       CHANGING tabela TYPE ANY TABLE.

  DATA: v_url      TYPE string,
        v_username TYPE string,
        v_password TYPE string.

  CLEAR: v_url,v_username, v_password.

  v_url      = p_url.
  v_username = p_username.
  v_password = p_password.


  cl_http_client=>create_by_url(
  EXPORTING url   =  |{ v_url }|
  IMPORTING client = e_http
  EXCEPTIONS argument_not_found = 1
           plugin_not_active  = 2
           internal_error     = 3 ).

  CALL METHOD e_http->request->set_header_field
    EXPORTING
      name  = '~request_method'
      value = 'GET'.
  CALL METHOD e_http->request->set_header_field
    EXPORTING
      name  = '~server_protocol'
      value = 'HTTP/1.1'.

  CALL METHOD e_http->request->set_header_field
    EXPORTING
      name  = 'Content-Type'
      value = 'application/json; charset=UTF-8'.

  CALL METHOD e_http->request->set_header_field
    EXPORTING
      name  = 'Accept'
      value = 'application/json'.


  e_http->authenticate( username = v_username  password = v_password ).

  CLEAR json_retorno.
  ob_web_service->zif_webservice~consultar(
      EXPORTING
        i_http                     = e_http
      IMPORTING
        e_reason                   = e_reason
      RECEIVING
        e_resultado                = json_retorno
      EXCEPTIONS
        http_communication_failure = 1
        http_invalid_state         = 2
        http_processing_failed     = 3
        http_invalid_timeout       = 4
        OTHERS                     = 5  ).

  IF vprodutor = abap_true.
    json_retorno = |[{ json_retorno }]|.
  ENDIF.

  /ui2/cl_json=>deserialize( EXPORTING json = json_retorno CHANGING data = tabela ).
  vprodutor = abap_false.

ENDFORM.

FORM z_consulta_indea_lista2 USING p_url      TYPE zauth_webservice-url
                                  p_username TYPE zauth_ws_0001-username
                                  p_password TYPE zauth_ws_0001-password
                       CHANGING tabela TYPE ANY TABLE.

  DATA: v_url      TYPE string,
        v_username TYPE string,
        v_password TYPE string.

  CLEAR: v_url,v_username, v_password.

  v_url      = p_url.
  v_username = p_username.
  v_password = p_password.


  cl_http_client=>create_by_url(
  EXPORTING url   =  |{ v_url }|
  IMPORTING client = e_http
  EXCEPTIONS argument_not_found = 1
           plugin_not_active  = 2
           internal_error     = 3 ).

  CALL METHOD e_http->request->set_header_field
    EXPORTING
      name  = '~request_method'
      value = 'GET'.
  CALL METHOD e_http->request->set_header_field
    EXPORTING
      name  = '~server_protocol'
      value = 'HTTP/1.1'.

  CALL METHOD e_http->request->set_header_field
    EXPORTING
      name  = 'Content-Type'
      value = 'application/json; charset=UTF-8'.

  CALL METHOD e_http->request->set_header_field
    EXPORTING
      name  = 'Accept'
      value = 'application/json'.


  e_http->authenticate( username = v_username  password = v_password ).

  CLEAR json_retorno.
  ob_web_service->zif_webservice~consultar(
      EXPORTING
        i_http                     = e_http
      IMPORTING
        e_reason                   = e_reason
      RECEIVING
        e_resultado                = json_retorno
      EXCEPTIONS
        http_communication_failure = 1
        http_invalid_state         = 2
        http_processing_failed     = 3
        http_invalid_timeout       = 4
        OTHERS                     = 5  ).

  IF vprodutor = abap_true.
    json_retorno = |[{ json_retorno }]|.
  ENDIF.

  /ui2/cl_json=>deserialize( EXPORTING json = json_retorno CHANGING data = tabela ).
  vprodutor = abap_false.

ENDFORM.


FORM f_exibe_bapi .

  CHECK p_nopop = abap_false.  "*-US191683-25.09.2025-#191683-JT-inicio

  CHECK tl_return[] IS NOT INITIAL.
  REFRESH tl_saida_exec.
  CLEAR: tl_saida_exec.


  LOOP AT tl_return.
    MOVE: tl_return-message TO tl_saida_exec-msg.
    APPEND tl_saida_exec.
  ENDLOOP.

  PERFORM montar_layout_msg.
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      it_fieldcat           = estrutura[]
      i_save                = 'A'
      i_screen_start_column = 3
      i_screen_start_line   = 3
      i_screen_end_column   = 100
      i_screen_end_line     = 13
    TABLES
      t_outtab              = tl_saida_exec.

ENDFORM.                    "F_EXIBE_BAPI

FORM montar_layout_msg .
  REFRESH estrutura.
  PERFORM montar_estrutura2 USING:
   1  'ZSDT0041'   'MSG'              'TL_SAIDA_EXEC' 'MSG'       'Msg de bapi'   '80'.
ENDFORM.                    " MONTAR_LAYOUT

FORM montar_estrutura2 USING VALUE(p_col_pos)       TYPE i
                            VALUE(p_ref_tabname)   LIKE dd02d-tabname
                            VALUE(p_ref_fieldname) LIKE dd03d-fieldname
                            VALUE(p_tabname)       LIKE dd02d-tabname
                            VALUE(p_field)         LIKE dd03d-fieldname
                            VALUE(p_scrtext_l)     LIKE dd03p-scrtext_l
                            VALUE(p_outputlen).

  DATA: x_contador TYPE string.
  CLEAR: wa_estrutura, x_contador.

  x_contador = strlen( p_scrtext_l ).

  wa_estrutura-fieldname     = p_field.
  wa_estrutura-tabname       = p_tabname.
  wa_estrutura-ref_tabname   = p_ref_tabname.
  wa_estrutura-ref_fieldname = p_ref_fieldname.
  wa_estrutura-key           = ' '.
  wa_estrutura-key_sel       = 'X'.
  wa_estrutura-col_pos       = p_col_pos.
  wa_estrutura-no_out        = ' '.
  wa_estrutura-seltext_s     = p_scrtext_l.
  wa_estrutura-seltext_m     = p_scrtext_l.
  wa_estrutura-seltext_l     = p_scrtext_l.
  IF p_outputlen IS INITIAL.
    wa_estrutura-outputlen     = x_contador.
  ELSE.
    wa_estrutura-outputlen     =  p_outputlen.
  ENDIF.

  APPEND wa_estrutura TO estrutura.

ENDFORM.                    " montar_estrutura
