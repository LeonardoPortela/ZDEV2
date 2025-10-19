FUNCTION z_sd_cte_da_nfe.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(P_DOCNUM) TYPE  J_1BDOCNUM
*"  CHANGING
*"     VALUE(MR_DOC) TYPE  J_1BNFDOC OPTIONAL
*"     VALUE(MR_LIN) TYPE  J_1BNFLIN OPTIONAL
*"     VALUE(MR_CTE) TYPE  J_1BNFE_ACTIVE OPTIONAL
*"     VALUE(MR_VBFA) TYPE  VBFA OPTIONAL
*"     VALUE(MR_VTTP) TYPE  VTTP OPTIONAL
*"     VALUE(MR_VBAK) TYPE  VBAK OPTIONAL
*"     VALUE(MR_VBRP) TYPE  VBRP OPTIONAL
*"     VALUE(MR_STATUS) TYPE  CHAR1
*"     VALUE(MR_CTE_LIN) TYPE  J_1BNFLIN OPTIONAL
*"     VALUE(MR_CTE_CAB) TYPE  J_1BNFDOC OPTIONAL
*"----------------------------------------------------------------------

  "  mr_status = 'C'. "Cancelado e Autorizado o Cancelamento
  "  mr_status = 'N'. "Não Cancelado e Autorizado para uso
  "  mr_status = 'E'. "Erro de Validação de CT-e
  "  mr_status = 'D'. "Conhecimento Denegado pelo SEFAZ
  "  mr_status = 'T'. "CT-e não enviado e Estornado
  "  mr_status = 'F'. "CT-e não enviado e não Estornado
  "  mr_status = 'M'. "Documento não é NF-e
  "  mr_status = 'L'. "Doc. de Trasp. Não localizado


  DATA: vg_vbeln TYPE vbeln_nach.

  CLEAR: mr_status.

  SELECT SINGLE * INTO mr_doc
    FROM j_1bnfdoc
   WHERE docnum EQ p_docnum.

  IF mr_doc-model NE '55'.
    mr_status = 'M'.
  ENDIF.

  CHECK mr_status NE 'M'.

  SELECT SINGLE * INTO mr_lin
    FROM j_1bnflin
   WHERE docnum EQ p_docnum.

  CHECK sy-subrc EQ 0.

  vg_vbeln = mr_lin-refkey(10).

  SELECT SINGLE * INTO mr_vbfa
    FROM vbfa
   WHERE vbeln   EQ vg_vbeln
     AND vbtyp_v EQ c_j
     and vbtyp_n EQ c_m
     and stufe   EQ '00'.

  CHECK sy-subrc EQ 0.

  SELECT SINGLE * INTO mr_vttp
    FROM vttp
   WHERE vbeln EQ mr_vbfa-vbelv.

  CHECK sy-subrc EQ 0.

  SELECT SINGLE * INTO mr_vbak
    FROM vbak
   WHERE tknum EQ mr_vttp-tknum.

  IF sy-subrc NE 0.
    mr_status = 'L'.
  ENDIF.
  CHECK mr_status NE 'L'.

  SELECT SINGLE * INTO mr_vbrp
    FROM vbrp
    WHERE aubel EQ mr_vbak-vbeln
* ---> S4 Migration - 17/07/2023 - LA
    AND DRAFT EQ SPACE.
* <--- S4 Migration - 17/07/2023 - LA

  CHECK sy-subrc EQ 0.

  SELECT SINGLE * INTO mr_cte_lin
    FROM j_1bnflin
   WHERE refkey EQ mr_vbrp-vbeln
     AND refitm EQ mr_vbrp-posnr.

  CHECK sy-subrc EQ 0.

  SELECT SINGLE * INTO mr_cte
    FROM j_1bnfe_active
   WHERE docnum EQ mr_cte_lin-docnum.

  SELECT SINGLE * INTO mr_cte_cab
    FROM j_1bnfdoc
   WHERE docnum EQ mr_cte_lin-docnum.

  IF mr_cte-cancel EQ 'X' AND mr_cte-docsta EQ '1'.
    mr_status = 'C'. "Cancelado e Autorizado o Cancelamento
  ELSEIF mr_cte-cancel IS INITIAL AND mr_cte-docsta EQ '1'.
    mr_status = 'N'. "Não Cancelado e Autorizado para uso
  ELSEIF ( mr_cte-docsta EQ '3' ).
    mr_status = 'E'. "Erro de Validação de CT-e
  ELSEIF ( mr_cte-docsta EQ '2' ).
    mr_status = 'D'. "Conhecimento Denegado pelo SEFAZ
  ELSEIF ( mr_cte-docsta IS INITIAL ) AND ( mr_cte_cab-cancel IS NOT INITIAL ).
    mr_status = 'T'. "CT-e não enviado e Estornado
  ELSEIF ( mr_cte-docsta IS INITIAL ) AND ( mr_cte_cab-cancel IS INITIAL ).
    mr_status = 'F'. "CT-e não enviado e não Estornado
  ENDIF.

  "MR_STATUS - (C)Cancelado(N)Não Cancelado(L)Transp. Não Localizado(M)Não é NF-e(E)CT-e com erro

ENDFUNCTION.
