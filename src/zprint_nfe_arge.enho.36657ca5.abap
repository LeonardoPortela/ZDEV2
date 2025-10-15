"Name: \PR:J_1A_WS_EXPORT_MONITOR\FO:HANDLE_HOTSPOT_CLICK\SE:BEGIN\EI
ENHANCEMENT 0 ZPRINT_NFE_ARGE.
*
  DATA: GS_ALV_REFRES_COND TYPE LVC_S_STBL.

  CASE fieldname.

    WHEN 'ZRFC_UNLOCK'.

      CLEAR: wa_j1acae_alv.
      READ TABLE it_j1acae_alv INDEX row_id ASSIGNING FIELD-SYMBOL(<FS_ALV>).
      IF <FS_ALV>-ZRFC_UNLOCK EQ ICON_UNLOCKED.
        UPDATE j_1acae
           SET CAE_STATUS = 'N'
               RFC_SENT_LOCK = SPACE
         WHERE BUKRS      EQ <FS_ALV>-BUKRS
           AND BRNCH      EQ <FS_ALV>-BRNCH
           AND CAE_REF    EQ <FS_ALV>-CAE_REF
           AND CAE_REFYR  EQ <FS_ALV>-CAE_REFYR
           AND CAE_REFTYP EQ <FS_ALV>-CAE_REFTYP
           AND BUDAT      EQ <FS_ALV>-BUDAT
           AND RFC_SENT_LOCK EQ ABAP_TRUE.

        IF SY-SUBRC IS INITIAL.
          COMMIT WORK.

          <FS_ALV>-CAE_STATUS = 'N'.
          <FS_ALV>-RFC_SENT_LOCK = SPACE.
          CLEAR: <FS_ALV>-ZRFC_UNLOCK.

          GS_ALV_REFRES_COND-ROW = ABAP_TRUE.
          GS_ALV_REFRES_COND-COL = ABAP_TRUE.

          IF ctl_alv_j1acae IS NOT INITIAL.
            CALL METHOD ctl_alv_j1acae->REFRESH_TABLE_DISPLAY
              EXPORTING
                IS_STABLE      = GS_ALV_REFRES_COND
                I_SOFT_REFRESH = ABAP_TRUE.
            CALL METHOD CL_GUI_CFW=>FLUSH.
          ENDIF.

        ENDIF.

      ENDIF.

    WHEN 'ZPRINT_NFE_AGR'.
      READ TABLE it_j1acae_alv INDEX row_id INTO wa_j1acae_alv.
      IF wa_j1acae_alv-CAE_STATUS EQ 'A'.
        SET PARAMETER ID 'VF' FIELD wa_j1acae_alv-cae_ref.
        SUBMIT ZSDY0004 WITH PFATNF = wa_j1acae_alv-cae_ref AND RETURN.
      else.
        MESSAGE S000(ZJ_1A_WS_CAE) WITH wa_j1acae_alv-cae_ref.
      ENDIF.
      EXIT.
  ENDCASE.


ENDENHANCEMENT.
