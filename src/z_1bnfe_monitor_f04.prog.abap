*&---------------------------------------------------------------------*
*&  Include           Z_1BNFE_MONITOR_F04
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  fill_it_fieldcatalog
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fill_it_fieldcatalog.

* Build the fieldcat according to DDIC structure J_1BNFE_ACTIVE:
  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      i_structure_name = 'J_1BNFE_ACTIVE'
    CHANGING
      ct_fieldcat      = it_fieldcatalog.

* Add an icon field for the Action Status
  wa_fieldcatalog-fieldname = 'STATUS'.
  wa_fieldcatalog-col_pos   = 1.
  wa_fieldcatalog-reptext   = TEXT-120.
  wa_fieldcatalog-outputlen = 2.
  APPEND wa_fieldcatalog TO it_fieldcatalog.

  IF sy-tcode EQ 'ZNFE' OR sy-tcode EQ 'ZMDFE'.
    wa_fieldcatalog-fieldname = 'CHAVE'."chave nf-e
    wa_fieldcatalog-col_pos   = 14.
    wa_fieldcatalog-reptext   = TEXT-206.
    wa_fieldcatalog-outputlen = 48.
    APPEND wa_fieldcatalog TO it_fieldcatalog.
  ENDIF.

  IF sy-tcode EQ 'ZCTE'.
* Add an icon field for the Action Status
    wa_fieldcatalog-fieldname = 'STATUS_CIOT'.
    wa_fieldcatalog-col_pos   = 2.
    wa_fieldcatalog-reptext   = TEXT-120.
    wa_fieldcatalog-outputlen = 2.
    APPEND wa_fieldcatalog TO it_fieldcatalog.

    wa_fieldcatalog-fieldname = 'CHAVECTE'."Chave CT-e
    wa_fieldcatalog-col_pos   = 15.
    wa_fieldcatalog-reptext   = TEXT-206.
    wa_fieldcatalog-outputlen = 48.
    APPEND wa_fieldcatalog TO it_fieldcatalog.

  ENDIF.

  IF sy-tcode EQ 'ZMDFE'.
* Add an icon field for the Action StatusFÇLSD~ÇFL
    wa_fieldcatalog-fieldname = 'ENCERRADO'.
    wa_fieldcatalog-col_pos   = 2.
    wa_fieldcatalog-reptext   = TEXT-203.
    wa_fieldcatalog-outputlen = 2.
    APPEND wa_fieldcatalog TO it_fieldcatalog.
  ENDIF.

  IF sy-tcode EQ 'ZNFE_TERC' OR sy-tcode EQ 'ZCFE_TERC'.
    "refkey
    wa_fieldcatalog-fieldname = 'REFKEY'.
    wa_fieldcatalog-col_pos   = 60.
    wa_fieldcatalog-reptext   = 'Número MIRO'.
    wa_fieldcatalog-outputlen = 15.
    APPEND wa_fieldcatalog TO it_fieldcatalog.
    "USNAM
    wa_fieldcatalog-fieldname = 'USNAM'.
    wa_fieldcatalog-col_pos   = 61.
    wa_fieldcatalog-reptext   = 'Usuário MIRO'.
    wa_fieldcatalog-outputlen = 15.
    APPEND wa_fieldcatalog TO it_fieldcatalog.
    "USNAM
    wa_fieldcatalog-fieldname = 'BELNR'.
    wa_fieldcatalog-col_pos   = 62.
    wa_fieldcatalog-reptext   = 'Doc.Contabil'.
    wa_fieldcatalog-outputlen = 10.
    APPEND wa_fieldcatalog TO it_fieldcatalog.
    "ZFBDT
    wa_fieldcatalog-fieldname = 'ZFBDT'.
    wa_fieldcatalog-col_pos   = 63.
    wa_fieldcatalog-reptext   = 'Dt.Vencimento'.
    wa_fieldcatalog-outputlen = 10.
    APPEND wa_fieldcatalog TO it_fieldcatalog.
    "AUGBL
    wa_fieldcatalog-fieldname = 'AUGDT'.
    wa_fieldcatalog-col_pos   = 64.
    wa_fieldcatalog-reptext   = 'Dt.Pagamento'.
    wa_fieldcatalog-outputlen = 10.
    APPEND wa_fieldcatalog TO it_fieldcatalog.
  ENDIF.

* Add an icon field for the Action Status
  wa_fieldcatalog-fieldname = 'ERRLOG'.
  wa_fieldcatalog-col_pos   = 3.
  CASE sy-tcode.
    WHEN 'ZNFE' OR 'ZNFE_TERC'.
      wa_fieldcatalog-reptext   = TEXT-130.
    WHEN 'ZCTE' OR 'ZCTE_TERC'.
      wa_fieldcatalog-reptext   = TEXT-200.
    WHEN 'ZMDFE'.
      wa_fieldcatalog-reptext   = TEXT-204.
  ENDCASE.
  wa_fieldcatalog-outputlen = 2.
  APPEND wa_fieldcatalog TO it_fieldcatalog.

  IF sy-tcode NE 'ZMDFE'.
    wa_fieldcatalog-fieldname = 'DT_ENVIO'.
    wa_fieldcatalog-col_pos   = 66.
    wa_fieldcatalog-reptext   = 'Dt. Envio e-mail'.
    wa_fieldcatalog-outputlen = 10.
    APPEND wa_fieldcatalog TO it_fieldcatalog.

    wa_fieldcatalog-fieldname = 'HR_ENVIO'.
    wa_fieldcatalog-col_pos   = 67.
    wa_fieldcatalog-reptext   = 'Hr. Envio e-mail'.
    wa_fieldcatalog-outputlen = 10.
    APPEND wa_fieldcatalog TO it_fieldcatalog.

    wa_fieldcatalog-fieldname = 'DS_EMAIL'.
    wa_fieldcatalog-col_pos   = 68.
    wa_fieldcatalog-reptext   = 'E-mail de envio'.
    wa_fieldcatalog-outputlen = 100.
    APPEND wa_fieldcatalog TO it_fieldcatalog.
  ENDIF.

* Modify field catalog
  LOOP AT it_fieldcatalog INTO wa_fieldcatalog.

*   Highlight Access-Key fields
    IF wa_fieldcatalog-fieldname = 'REGIO'.
*     START: field list of Access Key
      indic = c_x.
      wa_fieldcatalog-emphasize = c_grid_color_c300.
    ELSEIF indic = c_x AND wa_fieldcatalog-fieldname = 'CDV'.
*     END  : field list of Access Key
      wa_fieldcatalog-emphasize = c_grid_color_c300.
      CLEAR indic.
    ELSEIF indic = c_x.
      wa_fieldcatalog-emphasize = c_grid_color_c300.
    ENDIF.

*   Define column position
    CASE wa_fieldcatalog-fieldname.
      WHEN 'STATUS' OR 'STATUS_CIOT' OR 'ERRLOG'.
      WHEN 'ACTION_REQU'.
        wa_fieldcatalog-col_pos   = 2.
      WHEN 'DOCNUM'  OR
           'DOCSTA'  OR
           'SCSSTA'.
        wa_fieldcatalog-col_pos   = wa_fieldcatalog-col_pos + 3.
      WHEN 'MSSTAT'.
        wa_fieldcatalog-col_pos   = 7.
      WHEN 'CONTING_S'.
        wa_fieldcatalog-col_pos   = 8.
      WHEN 'CONTING' OR
           'CANCEL'  OR
           'CODE'.
        wa_fieldcatalog-col_pos   = wa_fieldcatalog-col_pos + 5.
      WHEN 'PRINTD'.
        wa_fieldcatalog-col_pos   = 12.
      WHEN 'DIRECT'.
        wa_fieldcatalog-col_pos   = 13.
      WHEN 'FORM'.
        wa_fieldcatalog-col_pos   = 14.
      WHEN 'REFNUM'.
        wa_fieldcatalog-col_pos   = wa_fieldcatalog-col_pos + 7.
      WHEN OTHERS.
        wa_fieldcatalog-col_pos   = wa_fieldcatalog-col_pos + 8.
    ENDCASE.

*   Hide column for Partner Type
    IF wa_fieldcatalog-fieldname = 'PARTYP'.
      wa_fieldcatalog-no_out = c_x.
    ENDIF.

*   Hide column for Reference Document
    IF wa_fieldcatalog-fieldname = 'REFNUM'.
      wa_fieldcatalog-no_out = c_x.
    ENDIF.

*   Activate single-click sensitivity (--> hotspot)
    IF wa_fieldcatalog-fieldname = 'ERRLOG' OR
       wa_fieldcatalog-fieldname = 'DOCNUM' OR
       wa_fieldcatalog-fieldname = 'NFNUM9' OR
       wa_fieldcatalog-fieldname = 'REFNUM'.
      wa_fieldcatalog-hotspot = 'X'.
    ENDIF.

*   Output of Field: Cancellation Reason Description          "1144194
    IF wa_fieldcatalog-fieldname = 'REASON_T'.              "1144194
      wa_fieldcatalog-outputlen = 20.                       "1144194
    ENDIF.                                                  "1144194

    MODIFY it_fieldcatalog FROM wa_fieldcatalog INDEX sy-tabix.

  ENDLOOP.

ENDFORM.                    " fill_it_fieldcatalog

*&---------------------------------------------------------------------*
*&      Form  fill_gs_variant
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fill_gs_variant .

  gs_variant-report      = sy-repid.
  gs_variant-handle      = space.
  gs_variant-log_group   = space.
  gs_variant-username    = space.
  gs_variant-variant     = space.
  gs_variant-text        = space.
  gs_variant-dependvars  = space.

ENDFORM.                    " fill_gs_variant
