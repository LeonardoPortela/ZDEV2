*  A M A G G I  E X P O R T A Ç Ã O  E  I M P O R T A Ç Ã O  L T D A.  *
************************************************************************
* Responsável ...: Amaggi Exportação & Importação Ltda                 *
* Data desenv ...: 31.01.2013                                          *
* Objetivo    ...: Smartform Ordem de Carregamento                     *
* Transação   ...: ZLES0050                                            *
* Autor       ...: Victor Hugo                                         *
* Chamado:    ...: 87244                                               *
************************************************************************

REPORT  zmmr0016.

TYPE-POOLS: zmmr1.
************************************************************************
* TABELAS                                                              *
************************************************************************
TABLES: lips.

************************************************************************
* ESTRUTURA                                                            *
************************************************************************
TYPES: BEGIN OF ty_lips,
        vbeln    TYPE lips-vbeln,
        posnr    TYPE lips-posnr,
        charg    TYPE lips-charg,
        lfimg    TYPE lips-lfimg,
        meins    TYPE lips-meins,
        vgbel    TYPE lips-vgbel,
        vrkme    TYPE lips-vrkme,
        umvkz    TYPE lips-umvkz,
        werks    TYPE lips-werks,
        matnr    TYPE lips-matnr,
        werks_x  TYPE lfa1-lifnr,

       END OF ty_lips,

       BEGIN OF ty_t001k,
         bwkey TYPE t001k-bwkey,
         bukrs TYPE t001k-bukrs,
       END OF ty_t001k,

       BEGIN OF ty_t001,
         bukrs TYPE t001-bukrs,
         butxt TYPE t001-butxt,
       END OF ty_t001,

       BEGIN OF ty_lfa1,
         werks TYPE lfa1-werks,
         name1 TYPE lfa1-name1,
         ort01 TYPE lfa1-ort01,
         stras TYPE lfa1-stras,
         regio TYPE lfa1-regio,
         lifnr TYPE lfa1-lifnr,
       END OF ty_lfa1,

       BEGIN OF ty_vbak,
         vbeln TYPE vbak-vbeln,
         kunnr TYPE vbak-kunnr,
       END OF ty_vbak,

       BEGIN OF ty_kna1,
         kunnr TYPE kna1-kunnr,
         name1 TYPE kna1-name1,
         stras TYPE kna1-stras,
         ort01 TYPE kna1-ort01,
         pstlz TYPE kna1-pstlz,
         regio TYPE kna1-regio,
         stcd1 TYPE kna1-stcd1,
         stcd2 TYPE kna1-stcd2,
         stcd3 TYPE kna1-stcd3,
         ort02 TYPE kna1-ort02,
       END OF ty_kna1,

       BEGIN OF ty_makt,
         matnr TYPE makt-matnr,
         maktx TYPE makt-maktx,
       END OF ty_makt,

       BEGIN OF ty_mch1,
        matnr     TYPE mch1-matnr,
        charg     TYPE mch1-charg,
        cuobj_bm  TYPE mch1-cuobj_bm,
        cuobj_aux TYPE ausp-objek,
       END OF ty_mch1,

       BEGIN OF ty_ausp,
          objek  TYPE ausp-objek,
          atinn  TYPE ausp-atinn,
          atwrt  TYPE ausp-atwrt,
       END OF ty_ausp,

       BEGIN OF ty_saida,
         bukrs    TYPE t001-bukrs,
         butxt    TYPE t001-butxt,
         f_name1  TYPE lfa1-name1,
         ort01    TYPE lfa1-ort01,
         stras    TYPE lfa1-stras,
         regio    TYPE lfa1-regio,
         dt_sys   TYPE sy-datum,
         mjahr    TYPE mseg-mjahr,
         nr_seq   TYPE zseq_oc,
         c_name1  TYPE kna1-name1,
         c_ort02  TYPE kna1-ort02,
         c_stras  TYPE kna1-stras,
         c_ort01  TYPE kna1-ort01,
         c_stcd1  TYPE kna1-stcd1,
         c_stcd3  TYPE kna1-stcd3,
         c_pstlz  TYPE kna1-pstlz,
         c_regio  TYPE kna1-regio,

         vgbel    TYPE lips-vgbel,
         lfimg    TYPE lips-lfimg,
         charg    TYPE lips-charg,
         embala   TYPE c LENGTH 8,
         pilha    TYPE c LENGTH 4,
         posicao  TYPE c LENGTH 4,
         maktx    TYPE makt-maktx,

       END OF ty_saida,

       BEGIN OF ty_zmmt0032,
         remessa       TYPE zmmt0032-remessa,
         zseq_oc       TYPE zmmt0032-zseq_oc,
         filial        TYPE zmmt0032-filial,
         cidade        TYPE zmmt0032-cidade,
         endereco      TYPE zmmt0032-endereco,
         estado        TYPE zmmt0032-estado,
         dt_ordem      TYPE zmmt0032-dt_ordem,
         safra         TYPE zmmt0032-safra,
         nome_cliente  TYPE zmmt0032-nome_cliente,
         nome_proprie  TYPE zmmt0032-nome_proprie,
         roteiro       TYPE zmmt0032-roteiro,
         municipio     TYPE zmmt0032-municipio,
         cpf_cnpj      TYPE zmmt0032-cpf_cnpj,
         ie            TYPE zmmt0032-ie,
         ordem         TYPE zmmt0032-ordem,
         cep           TYPE zmmt0032-cep,
         uf            TYPE zmmt0032-uf,
         quantidade    TYPE zmmt0032-quantidade,
         embalagem     TYPE zmmt0032-embalagem,
         lote          TYPE zmmt0032-lote,
         pilha         TYPE zmmt0032-pilha,
         posicao       TYPE zmmt0032-posicao,
         variedade     TYPE zmmt0032-variedade,
         nome_moto     TYPE zmmt0032-nome_moto,
         cpf_rg_moto   TYPE zmmt0032-cpf_rg_moto,
         placa_moto    TYPE zmmt0032-placa_moto,
       END OF ty_zmmt0032.





************************************************************************
* TABELAS INTERNA                                                      *
************************************************************************
DATA: it_lips        TYPE TABLE OF ty_lips,
      it_lips_aux    TYPE TABLE OF ty_lips,
      it_t001k       TYPE TABLE OF ty_t001k,
      it_t001        TYPE TABLE OF ty_t001,
      it_lfa1        TYPE TABLE OF ty_lfa1,
      it_vbak        TYPE TABLE OF ty_vbak,
      it_kna1        TYPE TABLE OF ty_kna1,
      it_makt        TYPE TABLE OF ty_makt,
      it_mch1        TYPE TABLE OF ty_mch1,
      it_ausp        TYPE TABLE OF ty_ausp,
      it_ausp_aux    TYPE TABLE OF ty_ausp,
      it_zmmt0032    TYPE TABLE OF ty_zmmt0032,
      it_saida       TYPE TABLE OF zmmr1_saida.


************************************************************************
* WORK AREA                                                            *
************************************************************************
DATA: wa_lips        TYPE ty_lips,
      wa_t001k       TYPE ty_t001k,
      wa_t001        TYPE ty_t001,
      wa_lfa1        TYPE ty_lfa1,
      wa_vbak        TYPE ty_vbak,
      wa_kna1        TYPE ty_kna1,
      wa_makt        TYPE ty_makt,
      wa_mch1        TYPE ty_mch1,
      wa_ausp        TYPE ty_ausp,
      wa_ausp_aux    TYPE ty_ausp,
      wa_zmmt0032    TYPE zmmt0032,
      wa_zmmt0032_s  TYPE ty_zmmt0032,
      wa_info        TYPE zmmr1_saida,
      wa_saida       TYPE zmmr1_saida.

************************************************************************
* VARIAVEIS                                                            *
************************************************************************
DATA: v_mod_nome(30) TYPE c,
      v_input,
      v_required,
      v_request,
      tabix TYPE sy-tabix,
      v_ok  TYPE c LENGTH 2,
      v_seq TYPE zmmt0032-zseq_oc.

************************************************************************
* SELECT SCREEN                                                        *
************************************************************************
SELECTION-SCREEN: BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
PARAMETERS:  p_vbeln TYPE lips-vbeln       MODIF ID bus,
             p_seq   TYPE zmmt0032-zseq_oc MODIF ID bus,
             p_safra TYPE mseg-mjahr MODIF ID opc,
             p_name1 TYPE kna1-name1 MODIF ID opc,
             p_stcd1 TYPE kna1-stcd1 MODIF ID opc,
             p_placa TYPE zplaca MODIF ID opc.
SELECTION-SCREEN: END OF BLOCK b1.

SELECTION-SCREEN: BEGIN OF BLOCK b3 WITH FRAME TITLE text-003.
PARAMETERS: p_gera RADIOBUTTON GROUP r1 USER-COMMAND asnum,
            p_imp  RADIOBUTTON GROUP r1,
            p_del  RADIOBUTTON GROUP r1.
SELECTION-SCREEN: END OF BLOCK b3.

************************************************************************
* INITIALIZATION                                                       *
************************************************************************
INITIALIZATION.
  v_mod_nome = 'OPC'.
  v_input    = '1'.

  PERFORM controla_tela USING v_input
                              v_required
                              v_request.
************************************************************************
* SELECTION-SCREEN OUTPUT                                              *
************************************************************************
AT SELECTION-SCREEN OUTPUT.

  CASE 'X'.
    WHEN: p_gera.

      v_mod_nome = 'OPC'.
      v_input    = '1'.

      PERFORM controla_tela USING v_input
                                  v_required
                                  v_request.
    WHEN: p_imp OR p_del.

      v_mod_nome = 'OPC'.
      v_input    = '0'.

      PERFORM controla_tela USING v_input
                                  v_required
                                  v_request.
  ENDCASE.

*&---------------------------------------------------------------------*
*&      Form  F_MODIFICAR_TELA
*&---------------------------------------------------------------------*
FORM controla_tela USING p_input
                         p_required
                         p_request.

  LOOP AT SCREEN.
    IF ( screen-group1 = v_mod_nome ).
      screen-active   = p_input.
      screen-required = p_required.
      screen-request  = p_request.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.
ENDFORM.                    " F_MODIFICAR_TELA

************************************************************************
* START-OF-SELECTION                                                   *
************************************************************************
START-OF-SELECTION.

  PERFORM validacao_campo.


  IF ( p_gera EQ 'X' ).
    PERFORM:  seleciona_dados.
  ELSEIF ( p_imp EQ 'X').
    PERFORM: imprimir_ordem.
  ELSEIF ( p_del EQ 'X' ).
    PERFORM: deleta_ordem.
  ENDIF.

*&---------------------------------------------------------------------*
*&      Form  VALIDACOES_CAMPO
*&---------------------------------------------------------------------*
FORM validacao_campo .

  IF NOT ( p_gera IS INITIAL ).

    IF ( p_vbeln IS INITIAL ).
      MESSAGE s899(mm) DISPLAY LIKE 'E' WITH 'É obrigatorio informar a Remessa'.
      STOP.
    ENDIF.

    IF ( p_safra IS INITIAL ).
      MESSAGE s899(mm) DISPLAY LIKE 'E' WITH 'É obrigatorio informar a Safra'.
      STOP.
    ENDIF.

    IF NOT ( p_seq IS INITIAL ).
      MESSAGE s899(mm) DISPLAY LIKE 'E' WITH 'Esta Ordem de Carregamento' 'ainda não foi criada'.
      STOP.
    ENDIF.

  ELSEIF NOT ( p_imp IS INITIAL ).
    IF ( p_vbeln IS INITIAL ) OR ( p_seq IS INITIAL ).
      MESSAGE s899(mm) DISPLAY LIKE 'E' WITH 'É obrigatorio informar a ' 'Remessa/Ordem de Carregamento'.
      STOP.
    ENDIF.
  ENDIF.
ENDFORM.                    " VALIDACOES_CAMPO
*&---------------------------------------------------------------------*
*&      Form  SELECIONA_DADOS
*&---------------------------------------------------------------------*
FORM seleciona_dados .

  " SELECIONAR DOCUMENTO SD FORNECIMENTO - DADOS DO ITEM
  SELECT vbeln posnr charg lfimg meins vgbel vrkme umvkz werks matnr
    FROM lips
    INTO TABLE it_lips
  WHERE vbeln EQ p_vbeln
    AND charg NE ''.


  IF ( sy-subrc NE 0 ).
    MESSAGE s899(mm) WITH 'Nenhuma remessa encontrada'.
  ENDIF.

  CLEAR: wa_lips, tabix.
  LOOP AT it_lips INTO wa_lips.
    tabix = sy-tabix.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = wa_lips-werks
      IMPORTING
        output = wa_lips-werks_x.


    MODIFY it_lips INDEX tabix FROM wa_lips TRANSPORTING werks_x.
    CLEAR: wa_lips, tabix.
  ENDLOOP.


  " SELECIONAR ÁREA DE AVALIAÇÃO
  SELECT bwkey bukrs
    FROM t001k
    INTO TABLE it_t001k
    FOR ALL ENTRIES IN  it_lips
  WHERE bwkey EQ it_lips-werks.

  CHECK NOT it_t001k[] IS INITIAL.

  " SELECIONAR EMPRESA
  SELECT bukrs butxt
    FROM t001
    INTO TABLE it_t001
    FOR ALL ENTRIES IN it_t001k
  WHERE bukrs EQ it_t001k-bukrs.

  CHECK NOT it_t001[] IS INITIAL.

  " SELECIONAR DADOS MESTRES DO FORNECEDORES
  SELECT werks name1 ort01 stras regio lifnr
    FROM lfa1
    INTO TABLE it_lfa1
    FOR ALL ENTRIES IN it_lips
  WHERE lifnr EQ it_lips-werks_x.

  " SELECIONAR DOCUMENTO DE VENDAS
  SELECT vbeln kunnr
    FROM vbak
    INTO TABLE it_vbak
    FOR ALL ENTRIES IN it_lips
  WHERE vbeln EQ it_lips-vgbel.

  " SELECIONAR DADOS MESTRE DO CLIENTE
  SELECT kunnr name1 stras ort01 pstlz regio stcd1 stcd2 stcd3 ort02
    FROM kna1
    INTO TABLE it_kna1
    FOR ALL ENTRIES IN it_vbak
  WHERE kunnr EQ it_vbak-kunnr.

  " SELECIONAR TEXTOS BREVES DE MATERIAL
  SELECT  matnr maktx
    FROM makt
    INTO TABLE it_makt
    FOR ALL ENTRIES IN it_lips
  WHERE matnr EQ it_lips-matnr.

  " SELECIONAR LOTES
  SELECT matnr charg cuobj_bm
    FROM mch1
    INTO TABLE it_mch1
    FOR ALL ENTRIES IN it_lips
  WHERE matnr EQ it_lips-matnr
    AND charg EQ it_lips-charg.

  LOOP AT it_mch1 INTO wa_mch1.
    tabix = sy-tabix.
    wa_mch1-cuobj_aux = wa_mch1-cuobj_bm.
    MODIFY it_mch1 INDEX tabix FROM wa_mch1 TRANSPORTING cuobj_aux.
  ENDLOOP.

  " Valores das modalidades das características
  SELECT objek atinn atwrt
    FROM ausp
    INTO TABLE it_ausp
    FOR ALL ENTRIES IN it_mch1
  WHERE objek EQ it_mch1-cuobj_aux
    AND ( atinn EQ '868' OR atinn EQ '867' ).

  PERFORM: gravar_dados.


ENDFORM.                    " SELECIONA_DADOS
*&---------------------------------------------------------------------*
*&      Form  GRAVAR_INFORMACOES
*&---------------------------------------------------------------------*
FORM gravar_dados.

  DATA: umvkz_aux TYPE c LENGTH 5,
        vrkme_sac TYPE c LENGTH 10,
        tipo      TYPE c LENGTH 10,
        seq_aux   TYPE sy-tabix,
        wa_zmmt0032_aux TYPE zmmt0032.


  CLEAR: wa_zmmt0032_aux.

  SELECT SINGLE * FROM zmmt0032 INTO wa_zmmt0032_aux WHERE remessa EQ p_vbeln.

  IF ( sy-subrc EQ 0 ).

    MESSAGE s899(mm) DISPLAY LIKE 'W' WITH 'Remessa já existe na' 'Ordem de Carregamento' wa_zmmt0032_aux-zseq_oc.
    CLEAR: wa_zmmt0032_aux.
    STOP.

  ELSE.



    LOOP AT it_lips INTO wa_lips.

      CALL FUNCTION 'NUMBER_GET_NEXT'
        EXPORTING
          nr_range_nr = '01'
          object      = 'ZID_OC'
        IMPORTING
          number      = wa_zmmt0032-zid_oc.


      wa_zmmt0032-ordem      = wa_lips-vgbel.
      wa_zmmt0032-quantidade = wa_lips-lfimg.
      wa_zmmt0032-lote       = wa_lips-charg.
      wa_zmmt0032-remessa    = wa_lips-vbeln.

      READ TABLE it_t001k INTO wa_t001k WITH KEY bwkey = wa_lips-werks.
      READ TABLE it_t001  INTO wa_t001  WITH KEY bukrs = wa_t001k-bukrs.
      READ TABLE it_lfa1  INTO wa_lfa1  WITH KEY lifnr = wa_lips-werks_x.

      umvkz_aux = wa_lips-umvkz.

      CALL FUNCTION 'CONVERSION_EXIT_CUNIT_OUTPUT'
        EXPORTING
          input          = wa_lips-vrkme
          language       = sy-langu
        IMPORTING
          output         = vrkme_sac
        EXCEPTIONS
          unit_not_found = 1
          OTHERS         = 2.
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.

      CONDENSE vrkme_sac NO-GAPS.
      CONDENSE umvkz_aux NO-GAPS.

      CONCATENATE vrkme_sac '' umvkz_aux INTO wa_zmmt0032-embalagem.

      wa_zmmt0032-filial   = wa_lfa1-name1.
      wa_zmmt0032-cidade   = wa_lfa1-ort01.
      wa_zmmt0032-endereco = wa_lfa1-stras.
      wa_zmmt0032-estado   = wa_lfa1-regio.
      wa_zmmt0032-dt_ordem = sy-datum.
      wa_zmmt0032-safra    = p_safra.

      SELECT SINGLE * FROM zmmt0032 INTO wa_zmmt0032_aux WHERE remessa EQ wa_lips-vbeln.

      IF ( sy-subrc EQ 0 ).
        wa_zmmt0032-zseq_oc = wa_zmmt0032_aux-zseq_oc.
      ELSE.

        CLEAR: v_seq.

        CALL FUNCTION 'NUMBER_GET_NEXT'
          EXPORTING
            nr_range_nr = '01'
            object      = 'ZSEQ_OC'
          IMPORTING
            number      = wa_zmmt0032-zseq_oc.

      ENDIF.

      READ TABLE it_vbak INTO wa_vbak WITH KEY vbeln = wa_lips-vgbel.
      READ TABLE it_kna1 INTO wa_kna1 WITH KEY kunnr = wa_vbak-kunnr.

      wa_zmmt0032-nome_cliente = wa_kna1-name1.
      wa_zmmt0032-nome_proprie = wa_kna1-ort02.
      wa_zmmt0032-roteiro      = wa_kna1-stras.
      wa_zmmt0032-municipio    = wa_kna1-ort01.
      wa_zmmt0032-cep          = wa_kna1-pstlz.
      wa_zmmt0032-uf           = wa_kna1-regio.

      IF NOT ( wa_kna1-stcd1 IS INITIAL ).
        wa_zmmt0032-cpf_cnpj     = wa_kna1-stcd1.
      ELSE.
        wa_zmmt0032-cpf_cnpj     = wa_kna1-stcd2.
      ENDIF.

      wa_zmmt0032-ie  = wa_kna1-stcd3.
      READ TABLE it_makt INTO wa_makt WITH KEY matnr = wa_lips-matnr.

      wa_zmmt0032-variedade = wa_makt-maktx.

      READ TABLE it_mch1 INTO wa_mch1 WITH KEY matnr = wa_lips-matnr
                                               charg = wa_lips-charg.

      READ TABLE it_ausp INTO wa_ausp WITH KEY objek = wa_mch1-cuobj_aux.

      CLEAR: it_ausp_aux.

      it_ausp_aux[] = it_ausp[].


      LOOP AT it_ausp_aux INTO wa_ausp_aux WHERE objek EQ wa_ausp-objek.

        CALL FUNCTION 'CONVERSION_EXIT_ATINN_OUTPUT'
          EXPORTING
            input  = wa_ausp_aux-atinn
          IMPORTING
            output = tipo.

        CASE tipo.
          WHEN: 'PILHA'.
            wa_zmmt0032-pilha   = wa_ausp_aux-atwrt.
          WHEN: 'POSICAO'.
            wa_zmmt0032-posicao = wa_ausp_aux-atwrt.
        ENDCASE.


        CLEAR: wa_ausp_aux.


      ENDLOOP.

      REFRESH: it_ausp_aux[].

      wa_zmmt0032-nome_moto   =  p_name1.
      wa_zmmt0032-cpf_rg_moto =  p_stcd1.
      wa_zmmt0032-placa_moto  =  p_placa.


      INSERT INTO zmmt0032 VALUES wa_zmmt0032.

      IF ( sy-subrc EQ 0 ).


        COMMIT WORK.
        v_ok = 'OK'.
        v_seq = wa_zmmt0032-zseq_oc.

        MESSAGE s899(mm) WITH 'Ordem de Carregamento' wa_zmmt0032-zseq_oc 'gravado com sucesso.'.

      ELSE.
        ROLLBACK WORK.
      ENDIF.

      CLEAR: wa_lips, wa_t001k, wa_t001, wa_lfa1, wa_vbak, wa_kna1, wa_makt, wa_mch1, wa_ausp, wa_zmmt0032, wa_zmmt0032_aux.

    ENDLOOP.

  ENDIF.

  IF NOT ( v_ok IS INITIAL ).
    PERFORM: imprimir_ordem.
  ENDIF.

ENDFORM.                    " GRAVAR_INFORMACOES
*&---------------------------------------------------------------------*
*&      Form  DELETA_ORDEM
*&---------------------------------------------------------------------*
FORM deleta_ordem .

  IF NOT ( p_vbeln IS INITIAL ) AND NOT ( p_seq IS INITIAL ).

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = p_seq
      IMPORTING
        output = p_seq.

    DELETE FROM zmmt0032 WHERE remessa EQ p_vbeln
                           AND zseq_oc EQ p_seq.
    IF ( sy-subrc EQ 0 ).
      COMMIT WORK.
      MESSAGE s899(mm) WITH 'Ordem de Carregamento' 'Cancelada com sucesso.'.
    ELSE.
      ROLLBACK WORK.
      MESSAGE s899(mm) DISPLAY LIKE 'W' WITH 'Ordem de Carregamento' 'não Cancelada.'.
    ENDIF.

  ELSE.
    MESSAGE s899(mm) DISPLAY LIKE 'E' WITH 'Informar o campo Remessa' 'e Ordem de Carregamento'.
  ENDIF.
ENDFORM.                    " DELETA_ORDEM
*&---------------------------------------------------------------------*
*&      Form  IMPRIMIR_ORDEM
*&---------------------------------------------------------------------*
FORM imprimir_ordem .

  REFRESH: it_zmmt0032[].
  CLEAR: wa_zmmt0032, v_ok.

  IF ( p_gera EQ 'X' ).
    p_seq = v_seq.
  ENDIF.


  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = p_seq
    IMPORTING
      output = p_seq.


  SELECT SINGLE * FROM zmmt0032 INTO wa_zmmt0032 WHERE remessa  EQ p_vbeln
                                                    AND zseq_oc EQ p_seq.

  IF ( sy-subrc EQ 0 ).
    wa_info-filial   = wa_zmmt0032-filial.
    wa_info-endereco = wa_zmmt0032-endereco.
    wa_info-cidade   = wa_zmmt0032-cidade.
    wa_info-estado   = wa_zmmt0032-estado.
    wa_info-zseq_oc  = wa_zmmt0032-zseq_oc.
    wa_info-dt_ordem = wa_zmmt0032-dt_ordem.
    wa_info-safra    = wa_zmmt0032-safra.

    wa_info-nome_cliente = wa_zmmt0032-nome_cliente.
    wa_info-nome_proprie = wa_zmmt0032-nome_proprie.
    wa_info-roteiro      = wa_zmmt0032-roteiro.
    wa_info-municipio    = wa_zmmt0032-municipio.

    wa_info-cpf_cnpj     = wa_zmmt0032-cpf_cnpj.
    wa_info-ie           = wa_zmmt0032-ie.
    wa_info-roteiro      = wa_zmmt0032-roteiro.
    wa_info-ordem        = wa_zmmt0032-ordem.
    wa_info-municipio    = wa_zmmt0032-municipio.
    wa_info-cep          = wa_zmmt0032-cep.
    wa_info-uf           = wa_zmmt0032-uf.


    wa_info-nome_moto   = wa_zmmt0032-nome_moto.
    wa_info-cpf_rg_moto = wa_zmmt0032-cpf_rg_moto.
    wa_info-placa_moto  = wa_zmmt0032-placa_moto.


    REFRESH: it_saida.

    SELECT remessa
           zseq_oc
           filial
           cidade
           endereco
           estado
           dt_ordem
           safra
           nome_cliente
           nome_proprie
           roteiro
           municipio
           cpf_cnpj
           ie
           ordem
           cep
           uf
           quantidade
           embalagem
           lote
           pilha
           posicao
           variedade
           nome_moto
           cpf_rg_moto
           placa_moto
    FROM zmmt0032
      INTO TABLE it_saida
    WHERE remessa EQ p_vbeln
      AND zseq_oc EQ p_seq.

  ELSE.
    MESSAGE s899(mm) DISPLAY LIKE 'E' WITH 'Dados não encontrado para' ' esta Remessa/Ordem'.
    STOP.
  ENDIF.

  PERFORM: executa_smartform.

ENDFORM.                    " IMPRIMIR_ORDEM
*&---------------------------------------------------------------------*
*&      Form  EXECUTA_SMARTFORM
*&---------------------------------------------------------------------*
FORM executa_smartform .

  DATA: vl_formname TYPE tdsfname,
        vl_name     TYPE rs38l_fnam.

  vl_formname = 'ZMMR0016'.


  CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
    EXPORTING
      formname           = vl_formname
    IMPORTING
      fm_name            = vl_name
    EXCEPTIONS
      no_form            = 1
      no_function_module = 2
      OTHERS             = 3.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    EXIT.
  ENDIF.

  CALL FUNCTION vl_name
    EXPORTING
      p_info           = wa_info
    TABLES
      it_saida         = it_saida
    EXCEPTIONS
      formatting_error = 1
      internal_error   = 2
      send_error       = 3
      user_canceled    = 4
      OTHERS           = 5.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.






ENDFORM.                    " EXECUTA_SMARTFORM
