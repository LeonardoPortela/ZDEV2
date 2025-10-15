CLASS lcl_document_selector IMPLEMENTATION.

  METHOD constructor.
    me->set_data( ).
  ENDMETHOD.

  METHOD set_data.

    DATA: ls_documents_for_change TYPE zsfi_change_document_yt0049,
          ls_yt0049_values_doc    TYPE zsfi_yt0049_values_doc.

    SELECT ctb_bukrs, ctb_belnr, ctb_gjahr, bstkd
      INTO TABLE @DATA(lt_zsdyt0049)
      FROM zsdyt0049
      WHERE ctb_gjahr  GE '2022'
      AND   ctb_belnr  NE @space
      AND   atribuicao EQ @space.

    CHECK sy-subrc IS INITIAL.

    SORT lt_zsdyt0049 BY ctb_bukrs ctb_belnr ctb_gjahr.
    DELETE ADJACENT DUPLICATES FROM lt_zsdyt0049 COMPARING ctb_bukrs ctb_belnr ctb_gjahr.

    SELECT *
      FROM bkdf
      INTO TABLE @DATA(lt_bkdf)
      FOR ALL ENTRIES IN @lt_zsdyt0049
      WHERE bukrs EQ @lt_zsdyt0049-ctb_bukrs
      AND   belnr EQ @lt_zsdyt0049-ctb_belnr
      AND   gjahr EQ @lt_zsdyt0049-ctb_gjahr.
    IF sy-subrc IS INITIAL.
      SORT lt_bkdf BY bukrs belnr gjahr.
    ENDIF.

    SELECT *
      FROM bkpf
      INTO TABLE @DATA(lt_bkpf)
      FOR ALL ENTRIES IN @lt_zsdyt0049
      WHERE bukrs EQ @lt_zsdyt0049-ctb_bukrs
      AND   belnr EQ @lt_zsdyt0049-ctb_belnr
      AND   gjahr EQ @lt_zsdyt0049-ctb_gjahr.
    IF sy-subrc IS INITIAL.
      SORT lt_bkpf BY bukrs belnr gjahr.
    ENDIF.

    SELECT *
      FROM bsec
      INTO TABLE @DATA(lt_bsec)
      FOR ALL ENTRIES IN @lt_zsdyt0049
      WHERE bukrs EQ @lt_zsdyt0049-ctb_bukrs
      AND   belnr EQ @lt_zsdyt0049-ctb_belnr
      AND   gjahr EQ @lt_zsdyt0049-ctb_gjahr.
    IF sy-subrc IS INITIAL.
      SORT lt_bsec BY bukrs belnr gjahr.
    ENDIF.

    SELECT *
      FROM bsed
      INTO TABLE @DATA(lt_bsed)
      FOR ALL ENTRIES IN @lt_zsdyt0049
      WHERE bukrs EQ @lt_zsdyt0049-ctb_bukrs
      AND   belnr EQ @lt_zsdyt0049-ctb_belnr
      AND   gjahr EQ @lt_zsdyt0049-ctb_gjahr.
    IF sy-subrc IS INITIAL.
      SORT lt_bsed BY bukrs belnr gjahr.
    ENDIF.

* ---> S4 Migration - 15/06/2023 - MA
*    SELECT *
*      FROM bseg
*      INTO TABLE @DATA(lt_bseg)
*      FOR ALL ENTRIES IN @lt_zsdyt0049
*      WHERE bukrs EQ @lt_zsdyt0049-ctb_bukrs
*      AND   belnr EQ @lt_zsdyt0049-ctb_belnr
*      AND   gjahr EQ @lt_zsdyt0049-ctb_gjahr.

    DATA: lt_fields TYPE fagl_t_field,
          lt_bseg   TYPE fagl_t_bseg.

    CALL FUNCTION 'FAGL_GET_BSEG_FOR_ALL_ENTRIES'
      EXPORTING
        it_for_all_entries = lt_zsdyt0049
        i_where_clause     = |BUKRS = IT_FOR_ALL_ENTRIES-BUKRS AND BELNR = IT_FOR_ALL_ENTRIES-BELNR AND GJAHR = IT_FOR_ALL_ENTRIES-GJAHR|
*       IT_FIELDLIST       = LT_FIELDS
      IMPORTING
        et_bseg            = lt_bseg
      EXCEPTIONS
        not_found          = 1.

    IF sy-subrc = 0 AND lines( lt_bseg ) > 0.
      MOVE-CORRESPONDING lt_bseg TO lt_bseg.
      sy-dbcnt = lines( lt_bseg ).
    ELSE.
      sy-subrc = 4.
      sy-dbcnt = 0.
    ENDIF.
* <--- S4 Migration - 15/06/2023 - MA
    IF sy-subrc IS INITIAL.
      SORT lt_bsed BY bukrs belnr gjahr.
    ENDIF.

    SELECT *
      FROM bset
      INTO TABLE @DATA(lt_bset)
      FOR ALL ENTRIES IN @lt_zsdyt0049
      WHERE bukrs EQ @lt_zsdyt0049-ctb_bukrs
      AND   belnr EQ @lt_zsdyt0049-ctb_belnr
      AND   gjahr EQ @lt_zsdyt0049-ctb_gjahr.
    IF sy-subrc IS INITIAL.
      SORT lt_bset BY bukrs belnr gjahr.
    ENDIF.

    LOOP AT lt_zsdyt0049 INTO DATA(ls_zsdyt0049).

      DATA(lt_bkdf_document) = lt_bkdf[].
      DATA(lt_bkpf_document) = lt_bkpf[].
      DATA(lt_bsec_document) = lt_bsec[].
      DATA(lt_bsed_document) = lt_bsed[].
      DATA(lt_bseg_document) = lt_bseg[].
      DATA(lt_bset_document) = lt_bset[].

      MOVE-CORRESPONDING ls_zsdyt0049 TO ls_yt0049_values_doc.

      ls_documents_for_change-yt0049_values_doc = ls_yt0049_values_doc.

      DELETE lt_bkdf_document WHERE belnr NE ls_zsdyt0049-ctb_belnr.
      ls_documents_for_change-bkdf[] = lt_bkdf_document[].

      DELETE lt_bkpf_document WHERE belnr NE ls_zsdyt0049-ctb_belnr.
      ls_documents_for_change-bkpf[] = lt_bkpf_document[].

      DELETE lt_bsec_document WHERE belnr NE ls_zsdyt0049-ctb_belnr.
      ls_documents_for_change-bsec[] = lt_bsec_document[].

      DELETE lt_bsed_document WHERE belnr NE ls_zsdyt0049-ctb_belnr.
      ls_documents_for_change-bsed[] = lt_bsed_document[].

      DELETE lt_bseg_document WHERE belnr NE ls_zsdyt0049-ctb_belnr.
      ls_documents_for_change-bseg[] = lt_bseg_document[].

      DELETE lt_bset_document WHERE belnr NE ls_zsdyt0049-ctb_belnr.
      ls_documents_for_change-bset[] = lt_bset_document[].

      APPEND ls_documents_for_change TO me->gt_documents_for_change[].

      FREE: lt_bkdf_document, lt_bkpf_document, lt_bsec_document, lt_bseg_document, ls_yt0049_values_doc, ls_documents_for_change, lt_bset_document.
    ENDLOOP.

    FREE: lt_zsdyt0049, lt_bkdf, lt_bsec, lt_bseg, lt_bsed, lt_bkpf, lt_bset.

  ENDMETHOD.

  METHOD get_documents_for_change.
    rv_documents_for_change[] = gt_documents_for_change[].
  ENDMETHOD.

ENDCLASS.

CLASS lcl_document_changer IMPLEMENTATION.

  METHOD constructor.
    me->go_document_selector = iv_document_selector.
  ENDMETHOD.

  METHOD change_values.

    me->gt_documents_for_change[] = go_document_selector->get_documents_for_change( ).

    LOOP AT gt_documents_for_change ASSIGNING FIELD-SYMBOL(<documents_for_change>).

      SPLIT <documents_for_change>-yt0049_values_doc-bstkd AT '-' INTO DATA(one) DATA(two) DATA(three) DATA(pedido).

      CONDENSE pedido NO-GAPS.

      LOOP AT <documents_for_change>-bseg ASSIGNING FIELD-SYMBOL(<bseg>).
        <bseg>-zuonr = pedido.
      ENDLOOP.

    ENDLOOP.

  ENDMETHOD.

  METHOD save_values.

    LOOP AT me->gt_documents_for_change INTO DATA(ls_documents_for_change).

      CALL FUNCTION 'CHANGE_DOCUMENT'
        TABLES
          t_bkdf = ls_documents_for_change-bkdf
          t_bkpf = ls_documents_for_change-bkpf
          t_bsec = ls_documents_for_change-bsec
          t_bsed = ls_documents_for_change-bsed
          t_bseg = ls_documents_for_change-bseg
          t_bset = ls_documents_for_change-bset.

    ENDLOOP.

  ENDMETHOD.

ENDCLASS.
