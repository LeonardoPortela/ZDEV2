FUNCTION zaa_get_data_imobilizado.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(I_ERDAT_INI) TYPE  ERDAT OPTIONAL
*"     VALUE(I_ERDAT_FIM) TYPE  ERDAT OPTIONAL
*"     VALUE(I_AEDAT_INI) TYPE  AEDAT OPTIONAL
*"     VALUE(I_AEDAT_FIM) TYPE  AEDAT OPTIONAL
*"  TABLES
*"      T_SAIDA TYPE  ZSDST0043
*"----------------------------------------------------------------------

  TYPES: BEGIN OF ty_anla,
           erdat        TYPE anla-erdat, "I
           aedat        TYPE anla-aedat, "I
           anln1        TYPE anla-anln1, "I
           anln2        TYPE anla-anln2, "I
           txt50        TYPE anla-txt50, "I
           txa50        TYPE anla-txa50, "I
           ord44        TYPE anla-ord44, "I
           sernr        TYPE anla-sernr, "I
           invnr        TYPE anla-invnr, "I
           invzu        TYPE anla-invzu, "I
           aktiv        TYPE anla-aktiv, "I
           deakt        TYPE anla-deakt, "I
           fiamt        TYPE anla-fiamt,  "I
           stadt        TYPE anla-stadt,  "I
           feins        TYPE anla-feins,  "I
           grufl        TYPE anla-grufl,  "I
           leart        TYPE anla-leart, "I
           anlkl        TYPE anla-anlkl, "I
           mcoa1        TYPE anla-mcoa1,
*           zimob_v      TYPE zaa005-zimob_v, "Z
*           zimob_p      TYPE zaa005-zimob_p, "Z
           werks        TYPE anlz-werks, "T
           kfzkz        TYPE anlz-kfzkz, "T
           gsber        TYPE anlz-gsber, "T
           stort        TYPE anlz-stort, "T
           ktext        TYPE t499s-ktext, "L
           raumn        TYPE anlz-raumn, "T
           kostl        TYPE anlz-kostl, "T
           txk20        TYPE ankt-txk20, "J
           pernr        TYPE anlz-pernr, "T
           gtext        TYPE tgsbt-gtext, "D
           ktext1       TYPE cskt-ktext, "K
           bukrs        TYPE t001-bukrs,  "E
           butxt        TYPE t001-butxt,  "E
           branch       TYPE j_1bbranch-branch, "B
           name         TYPE j_1bbranch-name,   "B
           kfzkz1       TYPE zaa001-kfzkz, "V
           cod_registro TYPE zaa001-cod_registro, "V
           bdatu        TYPE anlz-bdatu,
           adatu        TYPE anlz-adatu,
           herst        TYPE anla-herst,
         END OF ty_anla,

         BEGIN OF ty_zaa005,
           anln1   TYPE zaa005-anln1,
           anln2   TYPE zaa005-anln2,
           bukrs   TYPE zaa005-bukrs,
           gsber   TYPE zaa005-gsber,
           kostl   TYPE zaa005-kostl,
           gjahr   TYPE zaa005-gjahr,
           zimob_v TYPE zaa005-zimob_v,
           zimob_p TYPE zaa005-zimob_p,

         END OF ty_zaa005,
         BEGIN OF ty_z01,
           gjahr TYPE zaa005-gjahr,
         END OF ty_z01.

  TYPES: lr_range_te TYPE RANGE OF erdat,
         lr_range_ta TYPE RANGE OF aedat.
  DATA: it_anla        TYPE STANDARD TABLE OF ty_anla,
        it_zaa005_aux  TYPE STANDARD TABLE OF ty_zaa005,
        lw_table1      TYPE ty_z01,
        lw_table2      TYPE ty_z01,
        lw_saida       TYPE zsde0043,
        it_zaa005      TYPE STANDARD TABLE OF ty_zaa005,
        it_range_erdat TYPE RANGE OF anla-erdat,
        it_range_aedat TYPE RANGE OF anla-aedat.

  IF ( i_erdat_ini IS NOT INITIAL AND i_erdat_fim IS NOT INITIAL ) OR ( i_aedat_ini IS NOT INITIAL AND i_aedat_fim IS NOT INITIAL ).

    IF i_erdat_ini IS NOT INITIAL.
      it_range_erdat = VALUE lr_range_te( LET s = 'I' o = 'EQ' IN sign = 'I'
                                                              option = 'BT' ( low = i_erdat_ini high = i_erdat_fim )
                                                                            ).
    ENDIF.

    IF i_aedat_ini IS NOT INITIAL.
      it_range_aedat = VALUE lr_range_ta( LET s = 'I' o = 'EQ' IN sign = 'I'
                                                                option = 'BT' ( low = i_aedat_ini high = i_aedat_fim )
                                                                              ).
    ENDIF.

    SELECT i~erdat,
           i~aedat,
           i~anln1,
           i~anln2,
           i~txt50,
           i~txa50,
           i~ord44,
           i~sernr,
           i~invnr,
           i~invzu,
           i~aktiv,
           i~deakt,
           i~fiamt,
           i~stadt,
           i~feins,
           i~grufl,
           i~leart,
           i~anlkl,
           i~mcoa1,
           t~werks,
           t~kfzkz,
           t~gsber,
           t~stort,
           l~ktext,
           t~raumn,
           t~kostl,
           j~txk20,
           t~pernr,
           d~gtext,
           k~ktext,
           e~bukrs,
           e~butxt,
           b~branch,
           b~name,
           v~kfzkz,
           v~cod_registro,
           t~bdatu,
           t~adatu,
           i~herst
            INTO TABLE @it_anla
            FROM anla AS i
            INNER JOIN anlz AS t
             ON t~bukrs EQ i~bukrs
            AND t~anln1 EQ i~anln1
            AND t~anln2 EQ i~anln2
            INNER JOIN tgsbt AS d
            ON d~gsber EQ t~gsber
            AND d~spras EQ 'P'
            LEFT OUTER JOIN t499s AS l
             ON l~werks EQ t~werks
            AND l~stand EQ t~stort
            INNER JOIN ankt AS j
            ON j~spras EQ 'P'
            AND j~anlkl EQ i~anlkl
            INNER JOIN cskt AS k
            ON k~kostl EQ t~kostl
            AND k~datbi >= @sy-datum
            AND k~spras EQ 'P'
            INNER JOIN t001 AS e
            ON e~bukrs EQ i~bukrs
            LEFT OUTER JOIN t001w AS c
            ON c~werks EQ t~werks
            LEFT OUTER JOIN j_1bbranch AS b
            ON b~branch EQ c~j_1bbranch
            LEFT OUTER JOIN zaa001 AS v
            ON v~bukrs EQ i~bukrs
            AND v~anln1 EQ i~anln1
            AND v~anln2 EQ i~anln2
            WHERE i~erdat IN @it_range_erdat
            AND i~aedat IN @it_range_aedat.
    IF sy-subrc IS INITIAL .

*      SELECT bukrs, anln1, anln2, kfzkz
*        FROM zaa001
*         INTO TABLE @DATA(lt_zaa001)
*        FOR ALL ENTRIES IN @it_anla
*        WHERE bukrs EQ @it_anla-bukrs
*          AND anln1 EQ @it_anla-anln1
*          AND anln2 EQ @it_anla-anln2.
*      IF sy-subrc IS INITIAL.
*        SORT lt_zaa001 BY bukrs anln1 anln2.
*      ENDIF.

*      SELECT werks, stand, ktext
*        FROM t499s
*        INTO TABLE @DATA(lt_t499s)
*        FOR ALL ENTRIES IN @it_anla
*        WHERE werks EQ @it_anla-werks
*          AND stand EQ @it_anla-stort.
*      IF sy-subrc IS INITIAL.
*        SORT lt_t499s BY werks stand.
*      ENDIF.

      SELECT anln1
             anln2
             bukrs
             gsber
             kostl
             gjahr
             zimob_v
             zimob_p
       FROM zaa005 AS a1
       INTO TABLE it_zaa005_aux
        FOR ALL ENTRIES IN it_anla
       WHERE anln1 EQ it_anla-anln1
         AND anln2 EQ it_anla-anln2.
      "AND gjahr EQ ( SELECT MAX( gjahr ) FROM zaa005 WHERE anln1 EQ a1~anln1
      "                                                 AND anln2 EQ a1~anln2 ).
      IF sy-subrc IS INITIAL.
        SORT it_zaa005_aux BY anln1 ASCENDING
                              anln2 ASCENDING
                              gjahr DESCENDING.
        DELETE ADJACENT DUPLICATES FROM it_zaa005_aux COMPARING anln1 anln2.
      ENDIF.

      SORT it_anla BY bukrs anln1 anln2.

      LOOP AT it_anla INTO DATA(ls_anla).
        MOVE:
        ls_anla-txt50  TO lw_saida-ds_imobolizado,
        ls_anla-txa50  TO lw_saida-ds_imobolizado_comp,
        ls_anla-bdatu  TO lw_saida-dt_fim_validade,
        ls_anla-adatu  TO lw_saida-dt_ini_validade,
        ls_anla-erdat  TO lw_saida-dt_criacao,
        ls_anla-aedat  TO lw_saida-dt_atualizacao,
        ls_anla-anln1  TO lw_saida-cd_imob_numero,
        ls_anla-anln2  TO lw_saida-cd_imob_sub_numero,
        ls_anla-ord44  TO lw_saida-seguros,
        ls_anla-sernr  TO lw_saida-serie,
        ls_anla-invnr  TO lw_saida-chassi,
        ls_anla-invzu  TO lw_saida-inventario,
        ls_anla-aktiv  TO lw_saida-dt_incorpor,
        ls_anla-deakt  TO lw_saida-dt_desativ,
        ls_anla-fiamt  TO lw_saida-comarca,
        ls_anla-stadt  TO lw_saida-municipio,
        ls_anla-feins  TO lw_saida-area_tipo,
        ls_anla-grufl  TO lw_saida-area_terreno,
        ls_anla-leart  TO lw_saida-tp_leasing,
        ls_anla-anlkl  TO lw_saida-class_imobilizado,

*        ls_anla-txk20  TO lw_saida-ds_classe_imob,
        ls_anla-werks  TO lw_saida-centro,
        ls_anla-kfzkz  TO lw_saida-placa,
        ls_anla-gsber  TO lw_saida-divisao,
        ls_anla-gtext  TO lw_saida-ds_divisao,
        ls_anla-stort  TO lw_saida-localizacao,
        ls_anla-ktext  TO lw_saida-ds_localizacao,
        ls_anla-raumn  TO lw_saida-sala,
        ls_anla-pernr  TO lw_saida-nr_pessoal,
        ls_anla-kostl  TO lw_saida-centro_custo,
        ls_anla-ktext1 TO lw_saida-ds_ccusto,
        ls_anla-bukrs  TO lw_saida-empresa,
        ls_anla-butxt  TO lw_saida-ds_empresa,
        ls_anla-branch TO lw_saida-filial,
        ls_anla-name   TO lw_saida-ds_filial,
        ls_anla-kfzkz1 TO lw_saida-compl_placa,
        ls_anla-herst        TO lw_saida-fabricante,
        ls_anla-cod_registro TO lw_saida-compl_renavam,
        ls_anla-mcoa1        TO lw_saida-termo_pesquisa.


        lw_saida-ds_imobolizado_comp   = zcl_string=>tira_acentos( i_texto = zcl_string=>convert_to_utf8( i_texto = CONV #( ls_anla-txa50 ) ) ).
        lw_saida-ds_divisao  = zcl_string=>tira_acentos( i_texto = zcl_string=>convert_to_utf8( i_texto = CONV #( ls_anla-gtext ) ) ).
        lw_saida-ds_empresa = zcl_string=>tira_acentos( i_texto = zcl_string=>convert_to_utf8( i_texto = CONV #( ls_anla-butxt ) ) ).
        lw_saida-ds_localizacao       = zcl_string=>tira_acentos( i_texto = zcl_string=>convert_to_utf8( i_texto = CONV #( ls_anla-ktext ) ) ).
        lw_saida-ds_filial = zcl_string=>tira_acentos( i_texto = zcl_string=>convert_to_utf8( i_texto = CONV #( ls_anla-name ) ) ).
        lw_saida-ds_classe_imob = zcl_string=>tira_acentos( i_texto = zcl_string=>convert_to_utf8( i_texto = CONV #( ls_anla-txk20 ) ) ).
*        READ TABLE lt_zaa001 INTO DATA(lw_zaa001) WITH KEY bukrs = ls_anla-bukrs
*                                                           anln1 = ls_anla-anln1
*                                                           anln2 = ls_anla-anln2 BINARY SEARCH.
*        IF sy-subrc IS INITIAL.
*          lw_saida-compl_placa = lw_zaa001-kfzkz.
*        ENDIF.
*
*        READ TABLE lt_t499s INTO DATA(lw_t499s) WITH KEY werks = ls_anla-werks
*                                                         stand = ls_anla-stort BINARY SEARCH.
*        IF sy-subrc IS INITIAL.
*        lw_saida-ds_localizacao = lw_t499s-ktext.
*        ENDIF.
        CASE ls_anla-leart.
          WHEN '01'.
            lw_saida-desc_leart = 'Capital lease'.
          WHEN '02'.
            lw_saida-desc_leart = 'Operational Lease'.
          WHEN '03'.
            lw_saida-desc_leart = 'Garantia em financiamento'.
          WHEN '04'.
            lw_saida-desc_leart = 'Garantia em processo judicial'.
        ENDCASE.
        READ TABLE it_zaa005_aux INTO DATA(ls_zaa005) WITH KEY anln1 = ls_anla-anln1
                                                               anln2 = ls_anla-anln2 BINARY SEARCH.
        IF sy-subrc IS INITIAL.
          lw_saida-status_imob = ls_zaa005-zimob_v.
          lw_saida-plaqueta    = ls_zaa005-zimob_p.
        ENDIF.

        APPEND lw_saida TO t_saida.
        CLEAR lw_saida.
      ENDLOOP.

    ENDIF.


  ENDIF.



ENDFUNCTION.
