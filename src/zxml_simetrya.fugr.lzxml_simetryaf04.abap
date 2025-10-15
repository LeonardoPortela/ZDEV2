*----------------------------------------------------------------------*
***INCLUDE LZXML_SIMETRYAF04 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  ADD_MEMORANDO_NF_PROF
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_NF_PROD  text
*      -->P_WA_J_1BNFDOC2  text
*      -->P_WA_J_1BNFLIN2  text
*----------------------------------------------------------------------*
FORM add_memorando_nf_prof  TABLES it_memorando  STRUCTURE zdoc_memorando
                            USING  wa_nf_prod    TYPE zdoc_nf_produtor
                                   wa_j_1bnfdoc2 TYPE j_1bnfdoc
                                   wa_j_1bnflin2 TYPE j_1bnflin
                                   vg_memorando  TYPE z_memorando
                                   wa_memorando  TYPE zdoc_memorando.

  DATA:  wa_zdoc_exp   TYPE zdoc_exp,
         aux_str       TYPE string  ,
         wa_exportacao TYPE zreg_exportacao,
         wa_part       TYPE lfa1,
         wa_part_nota  TYPE lfa1.

  wa_memorando-nr_memorando    = vg_memorando.
  wa_memorando-tp_finalidade   = 'N'.
  wa_memorando-dt_emissao_memo = sy-datum.
  wa_memorando-status          = 'P'.
  wa_memorando-quantidade_memo = wa_nf_prod-menge.
  wa_memorando-remetente       = wa_j_1bnfdoc2-parid.

  IF ( wa_j_1bnfdoc2-land1 IS INITIAL ) OR ( wa_j_1bnfdoc2-regio IS INITIAL ).
    CALL FUNCTION 'Z_PARCEIRO_INFO'
      EXPORTING
        p_parceiro   = wa_j_1bnfdoc2-parid
        p_partype    = wa_j_1bnfdoc2-partyp
      CHANGING
        wa_info_part = wa_part_nota.

    wa_j_1bnfdoc2-land1 = wa_part_nota-land1.
    wa_j_1bnfdoc2-regio = wa_part_nota-regio.
  ENDIF.

  wa_memorando-pais_origem     = wa_j_1bnfdoc2-land1.
  wa_memorando-uf_origem       = wa_j_1bnfdoc2-regio.

  PERFORM responsavel_memorando USING wa_memorando-responsavel.

  SELECT SINGLE * INTO wa_zdoc_exp
    FROM zdoc_exp
   WHERE vbeln EQ wa_nf_prod-vbeln.

  IF sy-subrc EQ 0.

    "Informações de RE
    aux_str = wa_zdoc_exp-nr_registro_expo.
    REPLACE ALL OCCURRENCES OF REGEX '[^0-9]' IN aux_str WITH ''.
    wa_memorando-nr_dde = aux_str.
    SELECT SINGLE * INTO wa_exportacao
      FROM zreg_exportacao
     WHERE nr_registro_expo EQ wa_zdoc_exp-id_registro_expo.
    wa_memorando-dt_re = wa_exportacao-dt_registro_expo.

    CALL FUNCTION 'Z_PARCEIRO_INFO'
      EXPORTING
        p_parceiro   = wa_exportacao-id_importador
        p_partype    = 'V'
      CHANGING
        wa_info_part = wa_part.

    wa_memorando-pais_destino = wa_part-land1.

    "Informações de DDE
    aux_str = wa_zdoc_exp-nr_dde.
    REPLACE ALL OCCURRENCES OF REGEX '[^0-9]' IN aux_str WITH ''.
    wa_memorando-nr_re = aux_str.

*---> 06/06/2023 - Migração S4 - JS
*    SELECT SINGLE nr_dde
*      FROM zdde
*      INTO wa_memorando-dt_dde
*     WHERE id_dde EQ wa_zdoc_exp-id_dde.

    data: lv_nr_dde type char12.

    SELECT SINGLE nr_dde
      FROM zdde
      INTO lv_nr_dde
     WHERE id_dde EQ wa_zdoc_exp-id_dde.

    wa_memorando-dt_dde = conv #( lv_nr_dde ).
*<--- 06/06/2023 - Migração S4 - JS

  ENDIF.

  APPEND wa_memorando TO it_memorando.

  vg_memorando = vg_memorando + 1.

ENDFORM.                    " ADD_MEMORANDO_NF_PROF

*&---------------------------------------------------------------------*
*&      Form  RESPONSAVEL_MEMORANDO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM responsavel_memorando  USING  vg_responsavel TYPE char50.

  DATA: wa_setleaf  TYPE setleaf,
        vg_kokrs    TYPE kokrs,
        vg_kostl    TYPE kostl.

  "Opter Área de contabilidade de custos
  SELECT SINGLE * INTO wa_setleaf
    FROM setleaf
   WHERE setname EQ 'ZMEMORANDO_KOKRS'.

  vg_kokrs = wa_setleaf-valfrom.

  "Opter Centro de custo
  SELECT SINGLE * INTO wa_setleaf
    FROM setleaf
   WHERE setname EQ 'ZMEMORANDO_KOSTL'.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = wa_setleaf-valfrom
    IMPORTING
      output = vg_kostl.

  "Informações de Centro de Custo
  SELECT SINGLE verak INTO vg_responsavel
    FROM csks
   WHERE kokrs EQ vg_kokrs
     AND kostl EQ vg_kostl.

ENDFORM.                    " RESPONSAVEL_MEMORANDO

*&---------------------------------------------------------------------*
*&      Form  ADD_NOTA_MEMORANDO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM add_nota_memorando  TABLES  it_notas_memorando STRUCTURE zdoc_memo_nota
                         USING   wa_memorando TYPE zdoc_memorando
                                 wa_nf_prod   TYPE zdoc_nf_produtor
                                 wa_j_1bnfdoc2 TYPE j_1bnfdoc
                                 wa_j_1bnflin2 TYPE j_1bnflin.

  DATA: wa_nota TYPE zdoc_memo_nota.
  DATA: wa_makt TYPE makt.

  SELECT SINGLE * INTO wa_makt
    FROM makt
   WHERE matnr EQ wa_j_1bnflin2-matnr
     AND spras EQ sy-langu.

  wa_nota-nr_memorando = wa_memorando-nr_memorando.
  wa_nota-docnum       = wa_j_1bnflin2-docnum.
  wa_nota-itmnum       = wa_j_1bnflin2-itmnum.
  wa_nota-docdat       = wa_j_1bnfdoc2-docdat.
  wa_nota-model        = wa_j_1bnfdoc2-model.
  wa_nota-series       = wa_j_1bnfdoc2-series.
  wa_nota-nfnum        = wa_j_1bnfdoc2-nfnum.
  wa_nota-nfe          = wa_j_1bnfdoc2-nfe.
  wa_nota-nfenum       = wa_j_1bnfdoc2-nfenum.
  wa_nota-matnr        = wa_j_1bnflin2-matnr.
  wa_nota-nbm          = wa_j_1bnflin2-nbm.
  wa_nota-menge        = wa_nf_prod-menge.
  wa_nota-meins        = wa_j_1bnflin2-meins.
  wa_nota-maktx        = wa_makt-maktg.
  wa_nota-bukrs        = wa_j_1bnfdoc2-bukrs.
  wa_nota-branch       = wa_j_1bnfdoc2-branch.

ENDFORM.                    " ADD_NOTA_MEMORANDO
