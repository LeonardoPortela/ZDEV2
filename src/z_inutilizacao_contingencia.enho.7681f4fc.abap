"Name: \PR:SAPLJ_1B_NFE\FO:CALL_XNFE\SE:BEGIN\EI
ENHANCEMENT 0 Z_INUTILIZACAO_CONTINGENCIA.

  "Requisição de Inutilização
  IF ( p_scsstatus EQ c_request_for_skip      ) OR
     ( p_scsstatus EQ c_validation_error_skip ).

    IF ( p_acttab-tpemis  NE '1' ) AND ( p_acttab-model EQ '55' OR p_acttab-model EQ '57' ).

      p_acttab-docsta       = '3'.
      p_acttab-scssta       = '0'.
      p_acttab-action_requ  = '1'.
      p_acttab-msstat       = 'V'.

      SELECT SINGLE *
        FROM zsdt0254 INTO @DATA(wl_zsdt0254)
       WHERE docnum EQ @p_acttab-docnum.

      IF SY-SUBRC NE 0.

        SELECT SINGLE *
          FROM j_1bnfdoc INTO @DATA(wl_doc_inut)
         WHERE docnum EQ @p_acttab-docnum.

        IF SY-SUBRC EQ 0.
          CLEAR: wl_zsdt0254.

          wl_zsdt0254-docnum       = p_acttab-docnum.

          wl_zsdt0254-bukrs        = wl_doc_inut-bukrs.
          wl_zsdt0254-branch       = wl_doc_inut-branch.
          wl_zsdt0254-docdat       = wl_doc_inut-docdat.
          wl_zsdt0254-chave        = p_acttab-REGIO   && p_acttab-NFYEAR  && p_acttab-NFMONTH &&
                                     p_acttab-STCD1   && p_acttab-MODEL   && p_acttab-SERIE &&
                                     p_acttab-NFNUM9  && p_acttab-DOCNUM9 && p_acttab-CDV.

          wl_zsdt0254-dt_registro  = sy-datum.
          wl_zsdt0254-hr_registro  = sy-uzeit.
          wl_zsdt0254-us_registro  = sy-uname.

          MODIFY zsdt0254 FROM wl_zsdt0254.
        ENDIF.

      ENDIF.

      exit.

    ENDIF.

  endif.


ENDENHANCEMENT.
