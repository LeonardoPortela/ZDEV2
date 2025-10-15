*----------------------------------------------------------------------*
*   INCLUDE LMB_BUS2017FR0                                             *
*----------------------------------------------------------------------*
*---------------------------------------------------------------------*
*       FORM RETURN_HANDLING                                          *
*---------------------------------------------------------------------*
*       Tabelle RETURN wird aufgebaut.                                *
*---------------------------------------------------------------------*
*  -->  LOC_RETURN                                                    *
*---------------------------------------------------------------------*
FORM RETURN_HANDLING TABLES LOC_RETURN STRUCTURE BAPIRET2.

* Errorhandling ITEM
  LOOP AT T_EMSEG WHERE NOT MSGNO IS INITIAL.
    PERFORM SET_MSG_TO_BAPIRET2 TABLES LOC_RETURN
                                USING  T_EMSEG-MSGID
                                       T_EMSEG-MSGTY
                                       T_EMSEG-MSGNO
                                       T_EMSEG-MSGV1
                                       T_EMSEG-MSGV2
                                       T_EMSEG-MSGV3
                                       T_EMSEG-MSGV4
                                       'GOODSMVT_ITEM'
                                       t_emseg-tabix
                                       space.
  ENDLOOP.

  READ TABLE LOC_RETURN INDEX 1.
* Check, if there are errors on position level...
  IF SY-SUBRC IS INITIAL.
    GLOBAL_ERROR = TRUE.
  ENDIF.

  CHECK GLOBAL_ERROR = FALSE.

* Errorhandling HEADER
  IF ( F_TESTRUN is initial and S_EMKPF-SUBRC > 1 ) or
     ( F_TESTRUN = X        AND S_EMKPF-SUBRC > 5 ).
    GLOBAL_ERROR = TRUE.
    IF NOT S_EMKPF-MSGID IS INITIAL.
      PERFORM SET_MSG_TO_BAPIRET2 TABLES LOC_RETURN
                                  USING  S_EMKPF-MSGID
                                         S_EMKPF-MSGTY
                                         S_EMKPF-MSGNO
                                         S_EMKPF-MSGV1
                                         S_EMKPF-MSGV2
                                         S_EMKPF-MSGV3
                                         S_EMKPF-MSGV4
                                         'GOODSMVT_HEADER'
                                         0
                                         space.
    ELSE.
      PERFORM SY_MSG_TO_BAPIRET2 TABLES LOC_RETURN
                                 USING  0
                                        'GOODSMVT_HEADER'.
    ENDIF.
  ENDIF.

* Return of delivery no                                      "490833
  IF S_EMKPF-SUBRC = 1    AND                                "490833
     S_EMKPF-MSGID = 'L9' AND                                "490833
     ( S_EMKPF-MSGNO = '514' OR S_EMKPF-MSGNO = '515' ).     "490833
    S_EMKPF-MSGTY = 'S'.                                     "490833
    PERFORM SET_MSG_TO_BAPIRET2 TABLES LOC_RETURN            "490833
                                USING  S_EMKPF-MSGID         "490833
                                       S_EMKPF-MSGTY         "490833
                                       S_EMKPF-MSGNO         "490833
                                       S_EMKPF-MSGV1         "490833
                                       S_EMKPF-MSGV2         "490833
                                       S_EMKPF-MSGV3         "490833
                                       S_EMKPF-MSGV4         "490833
                                       'GOODSMVT_HEADER'     "490833
                                       0                     "490833
                                       space.                "490833
  ENDIF.                                                     "490833
ENDFORM.
