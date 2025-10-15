*----------------------------------------------------------------------*
*   INCLUDE LMB_BUS2017FS0                                             *
*----------------------------------------------------------------------*

*---------------------------------------------------------------------*
*      Form  SY_MSG_TO_BAPIRET2                                       *
*---------------------------------------------------------------------*
*      Create Message for table RETURN.                               *
*---------------------------------------------------------------------*
FORM SY_MSG_TO_BAPIRET2 TABLES LOC_RETURN    STRUCTURE BAPIRET2
                        USING  LOC_ROW       LIKE BAPIRET2-ROW
                               LOC_PARAMETER LIKE BAPIRET2-PARAMETER.
  CLEAR LOC_RETURN.
* Read Message from table T100 and set the parameters
  CALL FUNCTION 'BALW_BAPIRETURN_GET2'
       EXPORTING
            TYPE       = SY-MSGTY
            CL         = SY-MSGID
            NUMBER     = SY-MSGNO
            PAR1       = SY-MSGV1
            PAR2       = SY-MSGV2
            PAR3       = SY-MSGV3
            PAR4       = SY-MSGV4
            PARAMETER  = LOC_PARAMETER
            ROW        = LOC_ROW
       IMPORTING
            RETURN     = LOC_RETURN.
  APPEND LOC_RETURN .
ENDFORM.

*---------------------------------------------------------------------*
*      Form  SET_MSG_TO_BAPIRET2                                      *
*---------------------------------------------------------------------*
*      Create Message for table RETURN.                               *
*---------------------------------------------------------------------*
FORM SET_MSG_TO_BAPIRET2 TABLES LOC_RETURN    STRUCTURE BAPIRET2
                         USING  LOC_MSGID
                                LOC_MSGTY
                                LOC_MSGNO
                                LOC_MSGV1
                                LOC_MSGV2
                                LOC_MSGV3
                                LOC_MSGV4
                                LOC_PARAMETER LIKE BAPIRET2-PARAMETER
                                LOC_ROW       LIKE BAPIRET2-ROW
                                LOC_FIELD     LIKE BAPIRET2-FIELD.
  DATA: L_RETURN LIKE BAPIRET2.
  CLEAR LOC_RETURN.
  L_RETURN-TYPE       = LOC_MSGTY.
  L_RETURN-ID         = LOC_MSGID.
  L_RETURN-NUMBER     = LOC_MSGNO.
  L_RETURN-MESSAGE_V1 = LOC_MSGV1.
  L_RETURN-MESSAGE_V2 = LOC_MSGV2.
  L_RETURN-MESSAGE_V3 = LOC_MSGV3.
  L_RETURN-MESSAGE_V4 = LOC_MSGV4.
  CALL FUNCTION 'BALW_BAPIRETURN_GET2'
       EXPORTING
            TYPE       = L_RETURN-TYPE
            CL         = L_RETURN-ID
            NUMBER     = L_RETURN-NUMBER
            PAR1       = L_RETURN-MESSAGE_V1
            PAR2       = L_RETURN-MESSAGE_V2
            PAR3       = L_RETURN-MESSAGE_V3
            PAR4       = L_RETURN-MESSAGE_V4
            PARAMETER  = LOC_PARAMETER
            ROW        = LOC_ROW
            FIELD      = LOC_FIELD
       IMPORTING
            RETURN     = LOC_RETURN.
  APPEND LOC_RETURN.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  CHECK_RANGES
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_MATERIAL_RA  text
*      -->P_RETURN  text
*----------------------------------------------------------------------*
FORM CHECK_RANGES TABLES P_RANGES
                         P_RETURN STRUCTURE BAPIRET2.

DATA: RETURN LIKE BAPIRETURN1.
FIELD-SYMBOLS: <SIGN>,
               <OPTION>.

  ASSIGN COMPONENT: 'SIGN'   OF STRUCTURE P_RANGES TO <SIGN>,
                    'OPTION' OF STRUCTURE P_RANGES TO <OPTION>.

  LOOP AT P_RANGES.
    TRANSLATE <SIGN> TO UPPER CASE.
    TRANSLATE <OPTION> TO UPPER CASE.
    IF <SIGN> IS INITIAL OR <OPTION> IS INITIAL.
      <SIGN> = 'I'.
      <OPTION> = 'EQ'.
    ENDIF.
    MODIFY P_RANGES.
    CALL FUNCTION 'BALW_RANGES_CHECK'
         EXPORTING
              SIGN    = <SIGN>
              OPTION  = <OPTION>
         IMPORTING
              RETURN  = RETURN.
    IF NOT RETURN IS INITIAL.
      MOVE-CORRESPONDING RETURN TO P_RETURN.
      APPEND P_RETURN.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " CHECK_RANGES
