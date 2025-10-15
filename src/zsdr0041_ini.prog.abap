**/===========================================================================\*
**|      db      `7MMM.     ,MMF'      db       .g8"""bgd    .g8"""bgd `7MMF' |*
**|     ;MM:       MMMb    dPMM       ;MM:    .dP'     `M  .dP'     `M   MM   |*
**|    ,V^MM.      M YM   ,M MM      ,V^MM.   dM'       `  dM'       `   MM   |*
**|   ,M  `MM      M  Mb  M' MM     ,M  `MM   MM           MM            MM   |*
**|   AbmmmqMA     M  YM.P'  MM     AbmmmqMA  MM.    `7MMF'MM.    `7MMF' MM   |*
**|  A'     VML    M  `YM'   MM    A'     VML `Mb.     MM  `Mb.     MM   MM   |*
**| AMA.   .AMMA..JML. `'  .JMML..AMA.   .AMMA. `"bmmmdPY    `"bmmmdPY .JMML. |*
**/===========================================================================\*

**/===========================================================================\*
**|  Desenvolvedor:                                                           |*
**|    + Welgem Barbosa ( welgem.barbosa@amaggi.com.br )                      |*
**|                                                                           |*
**|  Funcional:                                                               |*
**|    + Paulo Quevedo ( paulo.quevedo@amaggi.com.br )                         |*
**|  Changelog:                                                               |*
**|                                                                           |*
**/===========================================================================\*

**/===========================================================================\*
**| Descrição:                                                                |*
**| JOB para Envio de e-mail de Nota com prazo para exportação                |*
**|                                                                           |*
**/===========================================================================\*

REPORT  zsdr0041_ini.

DATA: dt_inicial TYPE sy-datum,
      it_remover TYPE TABLE OF zsdt0261,
      tl_perioe  TYPE lxhme_range_date_t,
      wl_perioe  TYPE lxhme_range_date.

DATA: number           TYPE tbtcjob-jobcount,
      name             TYPE tbtcjob-jobname,
      user             TYPE sy-uname,
      cont             TYPE i,
      wa_0261          TYPE zsdt0261,
      print_parameters TYPE pri_params.

START-OF-SELECTION.

*  SELECT COUNT(*)
*    FROM tbtco
*   WHERE jobname LIKE 'MAGGI_EMAIL_NOTAS_EXPORT_PRAZO'
*   AND status EQ 'R'.
*
*  CHECK sy-subrc IS NOT INITIAL.

  FREE: tl_perioe.

  tl_perioe = VALUE #(
                      ( sign    = 'I' option  = 'GE' low = sy-datum - 240 )
                     ).

  dt_inicial = sy-datum - 240.

  SELECT dc~bukrs, dc~branch
    INTO TABLE @DATA(it_doc)
    FROM j_1bnfdoc AS dc
   WHERE dc~partyp EQ 'V'
     AND dc~direct EQ '1'
     AND dc~cancel EQ ' '
     AND dc~doctyp NE '5'
     AND dc~docdat >= @dt_inicial.

  SORT it_doc BY bukrs branch.
  DELETE ADJACENT DUPLICATES FROM it_doc COMPARING bukrs branch.

  IF it_doc IS NOT INITIAL.

    SELECT *
      FROM zsdt0261
      INTO TABLE it_remover.

    DO.

      LOOP AT it_doc INTO DATA(wa_doc).

        SELECT COUNT(*)
          FROM tbtco
          INTO cont
         WHERE jobname LIKE 'EXPORT_PRAZO_%'
         AND status IN ('S','R').

        CHECK cont <= 1.

        name = |EXPORT_PRAZO_{ wa_doc-bukrs }_{ wa_doc-branch }|.

        IF line_exists( it_remover[ jobname = name ] ).
          CONTINUE.
        ENDIF.

        wa_0261 = VALUE #( jobname = name strtdate = sy-datum strttime = sy-uzeit ).
        MODIFY zsdt0261 FROM wa_0261.
        COMMIT WORK.

        SELECT *
        FROM zsdt0261
        INTO TABLE it_remover.

        CALL FUNCTION 'JOB_OPEN'
          EXPORTING
            jobname          = name
          IMPORTING
            jobcount         = number
          EXCEPTIONS
            cant_create_job  = 1
            invalid_job_data = 2
            jobname_missing  = 3
            OTHERS           = 4.

        IF sy-subrc IS INITIAL.

          SUBMIT zsdr0041 WITH in_emp = wa_doc-bukrs
                          WITH in_bra = wa_doc-branch
                          VIA JOB name NUMBER number
                          AND RETURN.

          IF sy-subrc IS INITIAL.
            CALL FUNCTION 'JOB_CLOSE'
              EXPORTING
                jobcount             = number
                jobname              = name
                strtimmed            = abap_true
              EXCEPTIONS
                cant_start_immediate = 1
                invalid_startdate    = 2
                jobname_missing      = 3
                job_close_failed     = 4
                job_nosteps          = 5
                job_notex            = 6
                lock_failed          = 7
                OTHERS               = 8.
          ENDIF.

        ENDIF.

        WAIT UP TO 2 SECONDS.

      ENDLOOP.

      IF lines( it_remover ) EQ lines( it_doc ).
        DELETE FROM zsdt0261.
        EXIT.
      ENDIF.

    ENDDO.

  ENDIF.
