FUNCTION ZSD_INFO_NF_SE_AUX.
*"--------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(CHAVE_NFE) TYPE  ZDE_CHAVE_DOC_E
*"     REFERENCE(TIPO_NOTA) TYPE  J_1BNFDOC-FORM
*"     REFERENCE(DIRECAO) TYPE  J_1BNFDOC-DIRECT
*"  EXPORTING
*"     REFERENCE(E_RETORNO) TYPE  ZSD_INFO_NF_T
*"     REFERENCE(E_ERRO) TYPE  STRING
*"--------------------------------------------------------------------
* tables: ZSD_INFO_NF, ZSD_INFO_NF_PROD.
  TYPES: BEGIN OF ty_j_1bnfe_active,
           docnum    TYPE j_1bdocnum,
           nfnum9    TYPE j_1bnfnum9,
           serie     TYPE j_1bseries,
           stcd1     TYPE j_1bstcd1,
           chave_nfe TYPE zde_chave_doc_e,
         END OF ty_j_1bnfe_active.


  TYPES: BEGIN OF ty_zib_nfe_dist_ter,
           docnum     TYPE zib_nfe_dist_ter-docnum9,
           chave_nfe  TYPE zib_nfe_dist_ter-chave_nfe,
           numero     TYPE zib_nfe_dist_ter-numero,
           dt_emissao TYPE zde_dt_emissao,
           hr_emissao TYPE j_1bcretim,
           serie      TYPE zib_nfe_dist_ter-serie,
           forne_cnpj TYPE zib_nfe_dist_ter-forne_cnpj,
         END OF ty_zib_nfe_dist_ter.

  TYPES: BEGIN OF ty_produtos,
           docnum TYPE j_1bnflin-docnum,
           matnr  TYPE j_1bnflin-matnr,
           meins  TYPE j_1bnflin-meins,
           menge  TYPE j_1bnflin-menge,
           nbm    TYPE j_1bnflin-nbm,
           refkey TYPE j_1bnflin-refkey,
           maktx  TYPE makt-maktx,
         END OF ty_produtos.

  TYPES: BEGIN OF ty_produtos_ter,
           chave_nfe TYPE zde_chave_doc_e,
           matnr     TYPE zde_prod_codigo,
           maktx     TYPE makt-maktx,
           meins     TYPE zde_und_comerci,
           menge     TYPE j_1bnetqty,
         END OF ty_produtos_ter.


  DATA: it_json             TYPE  TABLE OF zsd_info_nf,
        it_j_1bnfe_active   TYPE TABLE OF ty_j_1bnfe_active,
        it_produtos         TYPE TABLE OF ty_produtos,
        it_zib_nfe_dist_ter TYPE TABLE OF ty_zib_nfe_dist_ter,
        it_produtos_ter     TYPE TABLE OF ty_produtos_ter,
        wa_j_1bnfe_active   TYPE ty_j_1bnfe_active,
        wa_produto          TYPE ty_produtos,
        wa_zib_nfe_dist_ter TYPE ty_zib_nfe_dist_ter,
        wa_produtos_ter     TYPE ty_produtos_ter,
        wa_json             TYPE zsd_info_nf,
        v_chave_nfe         TYPE zde_chave_doc_e,
        v_tipo_nota         TYPE  j_1bnfdoc-form,
        v_direcao           TYPE  j_1bnfdoc-direct,
        v_placa_cav         TYPE  vttk-text1,
        v_placa_reb1        TYPE  vttk-text2,
        v_placa_reb2        TYPE  vttk-text3,
        v_placa_reb3        TYPE  vttk-text4,
        v_data              TYPE  j_1bdocdat,
        v_form              TYPE  j_1bnfdoc-form,
        v_hora              TYPE 	j_1bnfdoc-cretim,
        v_vbelv             TYPE  vbfa-vbelv,
        v_tknum             TYPE vttk-tknum,
        linha               TYPE TABLE OF string,
        l_value             TYPE string.

  IF chave_nfe IS NOT INITIAL AND tipo_nota IS NOT INITIAL AND direcao IS NOT INITIAL.

    IF tipo_nota EQ 'P'.

      SELECT docnum
             nfnum9
             serie
             stcd1
      FROM j_1bnfe_active INTO CORRESPONDING FIELDS OF TABLE it_j_1bnfe_active
      WHERE regio     = chave_nfe(2) AND
            nfyear    = chave_nfe+2(2) AND
             nfmonth   = chave_nfe+4(2) AND
             stcd1     = chave_nfe+6(14) AND
             model     = chave_nfe+20(2) AND
             serie     = chave_nfe+22(3) AND
             nfnum9    = chave_nfe+25(9) AND
             docnum9   = chave_nfe+34(9) AND
             cdv       = chave_nfe+43(1).

      IF it_j_1bnfe_active[] IS NOT INITIAL.

        SELECT docnum matnr meins menge nbm refkey
        FROM j_1bnflin
        INTO CORRESPONDING FIELDS OF TABLE it_produtos
        FOR ALL ENTRIES IN it_j_1bnfe_active
        WHERE docnum EQ it_j_1bnfe_active-docnum.

        LOOP AT it_j_1bnfe_active INTO wa_j_1bnfe_active.

          SELECT SINGLE docdat cretim form FROM j_1bnfdoc INTO ( v_data, v_hora, v_form ) WHERE docnum = wa_j_1bnfe_active-docnum.
          IF  v_form IS NOT INITIAL.

            READ TABLE it_produtos INTO DATA(w_prod) INDEX 1.

            IF sy-subrc IS INITIAL.
              SELECT SINGLE vbelv FROM vbfa INTO v_vbelv WHERE vbeln  = w_prod-refkey AND vbtyp_v = 'J'.

              IF sy-subrc IS NOT INITIAL.
                SELECT SINGLE vbelv FROM vbfa INTO v_vbelv WHERE vbeln  = w_prod-refkey(10) AND vbtyp_v = 'J'.
              ENDIF.

              IF v_vbelv IS NOT INITIAL.
                SELECT  SINGLE tknum FROM vttp INTO v_tknum WHERE vbeln  = v_vbelv.

                IF sy-subrc IS INITIAL.
                  SELECT SINGLE text1 text2 text3 text4 FROM vttk INTO ( v_placa_cav, v_placa_reb1, v_placa_reb2, v_placa_reb3 ) WHERE tknum = v_tknum.
                ENDIF.
              ENDIF.

              "Dados do fornecedor.
              SELECT SINGLE * FROM j_1bnfdoc INTO @DATA(w_j_1bnfdoc) WHERE docnum EQ @wa_j_1bnfe_active-docnum.

            ENDIF.

            wa_json-chave_nfe = chave_nfe.
            wa_json-nfnum9 = wa_j_1bnfe_active-nfnum9.
            wa_json-serie = wa_j_1bnfe_active-serie .
            wa_json-stcd1 = w_j_1bnfdoc-cgc."chave_nfe+6(14).
            wa_json-forn_razao = w_j_1bnfdoc-name1.
            wa_json-docdat = v_data .
            wa_json-hora = v_hora .
            wa_json-placa_cav = v_placa_cav(7) .
            wa_json-placa_reb1 = v_placa_reb1(7) .
            wa_json-placa_reb2 = v_placa_reb2(7) .
            wa_json-placa_reb3 = v_placa_reb3(7) .
            wa_json-tipo_nota =  tipo_nota .
            wa_json-direcao = direcao .
            wa_json-ort01 = w_j_1bnfdoc-ort01."Local / Municipio
            wa_json-ort02 = w_j_1bnfdoc-ort02."Bairro
            wa_json-regio = w_j_1bnfdoc-regio."Região (estado federal, estado federado, província, condado)
            wa_json-land1 = w_j_1bnfdoc-land1."Chave do país
            wa_json-pstlz = w_j_1bnfdoc-pstlz."Código postal
            wa_json-pfach = w_j_1bnfdoc-pfach."CxPostal
            wa_json-pstl2 = w_j_1bnfdoc-pstl2."Código postal da cx.postal


            LOOP AT it_produtos INTO  wa_produto WHERE docnum EQ wa_j_1bnfe_active-docnum.

              "Selecionar descrição do produto.
              IF wa_produto-matnr IS NOT INITIAL.
                SELECT SINGLE maktx FROM makt INTO wa_produto-maktx WHERE matnr EQ wa_produto-matnr AND spras EQ sy-langu.
              ENDIF.


              APPEND l_value TO linha.
              APPEND VALUE #( matnr = wa_produto-matnr
                               desc_mat = wa_produto-maktx
                               meins = wa_produto-meins
                               menge = wa_produto-menge
                               ncm = wa_produto-nbm ) TO wa_json-produtos.

            ENDLOOP.
            APPEND wa_json TO it_json.
          ELSE.
            e_erro = 'Formulario não informado'.
          ENDIF.
          CLEAR: v_data, v_hora, v_form, w_prod, v_vbelv, v_tknum,  v_placa_cav, v_placa_reb1, v_placa_reb2, v_placa_reb3, w_j_1bnfdoc.
        ENDLOOP.
      ELSE.
        e_erro = 'Chave NFE não encontrada.'.
      ENDIF.

    ELSE.

      "terceiro
      SELECT
        chave_nfe
        numero
        dt_emissao
        hr_emissao
        serie
        forne_cnpj
         FROM zib_nfe_dist_ter INTO CORRESPONDING FIELDS OF TABLE it_zib_nfe_dist_ter WHERE chave_nfe = chave_nfe.

      IF it_zib_nfe_dist_ter IS NOT INITIAL.


        SELECT matnr meins menge
        FROM zib_nfe_dist_itm
        INTO CORRESPONDING FIELDS OF TABLE it_produtos_ter
        FOR ALL ENTRIES IN it_zib_nfe_dist_ter
        WHERE chave_nfe = it_zib_nfe_dist_ter-chave_nfe.


        LOOP AT it_zib_nfe_dist_ter INTO wa_zib_nfe_dist_ter.

          READ TABLE it_produtos_ter INTO DATA(w_prod_ter) INDEX 1.



          wa_json-chave_nfe = chave_nfe.
          wa_json-nfnum9  = wa_zib_nfe_dist_ter-numero.
          wa_json-serie   = wa_zib_nfe_dist_ter-serie .
          wa_json-stcd1   = wa_zib_nfe_dist_ter-forne_cnpj.
          wa_json-docdat  = wa_zib_nfe_dist_ter-dt_emissao .
          wa_json-hora    = wa_zib_nfe_dist_ter-hr_emissao.
          wa_json-tipo_nota =  tipo_nota .
          wa_json-direcao = direcao .


          LOOP AT it_produtos_ter INTO  wa_produtos_ter WHERE chave_nfe EQ wa_zib_nfe_dist_ter-chave_nfe.
            "Selecionar descrição do produto.
            IF wa_produtos_ter-matnr IS NOT INITIAL.
              SELECT SINGLE maktx FROM makt INTO wa_produtos_ter-maktx WHERE matnr EQ wa_produtos_ter-matnr.
            ENDIF.


            APPEND VALUE #(  matnr = wa_produtos_ter-matnr
                          desc_mat = wa_produto-maktx
                             meins = wa_produtos_ter-matnr
                             menge = wa_produtos_ter-menge ) TO wa_json-produtos.

          ENDLOOP.
          CLEAR:  w_prod.

          APPEND wa_json TO it_json.
        ENDLOOP.
      ELSE.
        e_erro = 'Chave NFE Não encontrada!'.
      ENDIF.

    ENDIF.

  ENDIF.

  e_retorno =  it_json.


ENDFUNCTION.
