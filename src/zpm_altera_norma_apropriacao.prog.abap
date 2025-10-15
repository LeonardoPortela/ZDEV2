*&---------------------------------------------------------------------*
*& Include          ZPM_ALTERA_NORMA_APROPRIACAO
*&---------------------------------------------------------------------*
IF sy-tcode EQ 'IW31' OR sy-tcode EQ 'IW32'.

  TYPES: BEGIN OF ty_cobrb_buf.
           INCLUDE STRUCTURE cobrb.
  TYPES:   uflag LIKE dkobr-upd_flag,
         END OF ty_cobrb_buf.

  TYPES: tb_cobrb TYPE TABLE OF ty_cobrb_buf.

  FIELD-SYMBOLS: <fs_iloa>  TYPE iloa,
                 <fs_cobrb> TYPE tb_cobrb,
                 <fs_werks> TYPE caufv-werks.

  ASSIGN ('(SAPLKOBS)GT_COBRB_BUF[]') TO <fs_cobrb>.
  ASSIGN ('(SAPLCOIH)ILOA') TO <fs_iloa>.
  ASSIGN ('(SAPLCOIH)CAUFVD-WERKS') TO <fs_werks>.

  IF <fs_cobrb> IS ASSIGNED AND <fs_iloa> IS ASSIGNED.

    IF <fs_cobrb> IS NOT INITIAL.

      IF <fs_iloa>-kostl+7(3) = '196'.

        IF <fs_iloa>-aufnr IS NOT INITIAL.
          SELECT *
            UP TO 1 ROWS
            INTO @DATA(wa_aufk)
            FROM aufk
            WHERE aufnr EQ @<fs_iloa>-aufnr
              AND autyp EQ '01'
              AND ( auart EQ 'ZSTA' OR auart EQ 'ZOAN' )
              AND phas1 EQ @abap_true
              AND werks EQ @<fs_werks>.
          ENDSELECT.
          IF sy-subrc IS INITIAL.
            LOOP AT <fs_cobrb> ASSIGNING FIELD-SYMBOL(<lw_cobrb>).
              <lw_cobrb>-aufnr = <fs_iloa>-aufnr.
              CLEAR <lw_cobrb>-kostl.
              <lw_cobrb>-konty = 'OR'.
            ENDLOOP.
          ENDIF.

        ENDIF.

      ENDIF.

    ENDIF.

  ENDIF.

ENDIF.
