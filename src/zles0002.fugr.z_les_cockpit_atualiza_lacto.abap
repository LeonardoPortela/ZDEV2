FUNCTION z_les_cockpit_atualiza_lacto.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(P_TRANSPORTADOR) TYPE  TDLNR
*"     REFERENCE(P_POSTO) TYPE  LIFNR
*"     REFERENCE(P_LOTE) TYPE  CHAR10
*"     REFERENCE(P_CHVID) TYPE  ZCHVID
*"     REFERENCE(P_CTAFRETE) TYPE  EXTI2 OPTIONAL
*"     REFERENCE(P_CONHECIMENTO) TYPE  EXTI1 OPTIONAL
*"     REFERENCE(P_ACDCTIPO) TYPE  ZACDCTIPO OPTIONAL
*"     REFERENCE(P_DOCSAP) TYPE  BELNR_D OPTIONAL
*"     REFERENCE(P_AT_CC_CF_ACDTP) TYPE  CHAR01 OPTIONAL
*"----------------------------------------------------------------------

  IF p_at_cc_cf_acdtp IS INITIAL.

    UPDATE zlest0016
       SET ctafrete     = p_ctafrete
           conhecimento = p_conhecimento
     WHERE transportador = p_transportador
       AND posto         = p_posto
       AND lote          = p_lote
       AND chvid         = p_chvid.

    UPDATE zlest0022
       SET ctafrete     = p_ctafrete
           conhecimento = p_conhecimento
     WHERE transportador = p_transportador
       AND posto         = p_posto
       AND lote          = p_lote
       AND chvid         = p_chvid
       AND ctlglancto    = 'VC'.

  ELSE.

    UPDATE zlest0022
       SET ctafrete     = p_ctafrete
           conhecimento = p_conhecimento
           acdctipo     = p_acdctipo
     WHERE transportador = p_transportador
       AND posto         = p_posto
       AND lote          = p_lote
       AND chvid         = p_chvid
       AND ctlglancto    = 'VC'
       AND docsap        = p_docsap.

  ENDIF.

  COMMIT WORK.

ENDFUNCTION.
