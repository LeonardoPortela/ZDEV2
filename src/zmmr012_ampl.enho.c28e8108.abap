"Name: \PR:SAPLFACI\FO:FI_DOCUMENT_POST\SE:BEGIN\EI
ENHANCEMENT 0 ZMMR012_AMPL.
***Ampliação para mudar o preço do dolar
***Faz parte do programa ZMMR012
***Projeto Evoluir GAP_MM06 - Transferência Centro a Fixar para Fixo MB1C
DATA: v_field(30) TYPE c,
      v_DMBE2     type BSEG-DMBE2.

DATA: wa_ZMMT0006 type ZMMT0006,
      wa_xbseg    type bseg.

FIELD-SYMBOLS <fs_ZMMT0006> TYPE ZMMT0006.

***Verifica se o programa de chamada é o ZMMR012
  IF sy-cprog = 'ZMMR012'.

***Faz rotina para buscar valor da memória
    v_field = '(ZMMR012)WA_ZMMT0006'.
    ASSIGN (v_field) TO <fs_ZMMT0006>.
      IF sy-subrc = 0.
        wa_zmmt0006 = <fs_ZMMT0006>.
      ELSE.
        UNASSIGN <fs_ZMMT0006>.
        CLEAR wa_zmmt0006.
      ENDIF.

***Faz rotina para substituição do dolar
    IF wa_zmmt0006 IS NOT INITIAL.
      LOOP AT xbseg INTO wa_xbseg.
        IF wa_xbseg-bschl = '81' OR
           wa_xbseg-bschl = '91'.
          if wa_zmmt0006-vr_material_usd GT 0.
            v_dmbe2 = wa_zmmt0006-vr_material_usd.

            "Atribui valor do dolar para a tabela XBSEG
            wa_xbseg-dmbe2 = v_dmbe2.
            "Modifica tabela XBSEG com o novo valor
            "Essa mudança ira ser replicada para a tabela BSEG
            MODIFY xbseg FROM wa_xbseg.
          endif.
        ENDIF.
      ENDLOOP.
    ENDIF.

  ENDIF.

ENDENHANCEMENT.
