*----------------------------------------------------------------------*
*   INCLUDE HBRCVTR0TOP                                                *
*----------------------------------------------------------------------*

DATA: BEGIN OF COLL_TAB OCCURS 0.
DATA: BUKRS         LIKE P0001-BUKRS,
      WERKS         LIKE P0001-WERKS,
      BTRTL         LIKE P0001-BTRTL,
      CODAQ         TYPE SUBTY,
      PERNR         LIKE PERNR-PERNR,
      PERSG         LIKE P0001-PERSG,
      PERSK         LIKE P0001-PERSK,
      ENAME         LIKE P0002-CNAME,
      TRANS         LIKE P0410-TRANS,
      TDESC         LIKE Q0410-TDESC,
      TVIAG         LIKE P0410-TVIAG,
      AUSE          TYPE I,
      FERI          TYPE I,
      DAYS          TYPE I,
      NBTRP         TYPE I,
      VALTT         LIKE PC207-BETRG,
      VALTT_TOT     LIKE PC207-BETRG,
      SALARY        LIKE PC207-BETRG,    " contractual  or actual
                                         " salary
      WAERS         LIKE T500C-WAERS,
      PERCENTAGE    LIKE T7BR1B-TRDED,   " percentage the employee
                                         " will pay for the tickets
      TOTAL_TICKETS LIKE PC207-BETRG,    " total value of tickets
      MAX_DISC_PERM LIKE PC207-BETRG,    " maximum discount
                                         " permitted = salary * %
      REAL_DISC     LIKE PC207-BETRG,    " real discount (total of
                                         " tickets or % over salary)
      INF15_EXS     TYPE I,              " indicates how many times the
                                         " WT MVTR occurs for the
                                         " same period
      CPF           TYPE P0465-CPF_NR.
DATA: END OF COLL_TAB.

DATA: BEGIN OF ALV_TAB OCCURS 0.
DATA: BUKRS     LIKE P0001-BUKRS,
      BUKRST    LIKE HRCA_COMPANY-COMP_NAME,
      WERKS     LIKE P0001-WERKS,
      BTRTL     LIKE P0001-BTRTL,
      BTRTLT    LIKE T001P-BTEXT,
      PERNR     LIKE PERNR-PERNR,
      ENAME     LIKE P0002-CNAME,
      PERSG     LIKE P0001-PERSG,
      PERSGT    LIKE T501T-PTEXT,
      PERSK     LIKE P0001-PERSK,
      PERSKT    LIKE T503T-PTEXT,
      TRANS     LIKE P0410-TRANS,
      TRANST    LIKE Q0410-TDESC,
      TVIAG     LIKE P0410-TVIAG,
      DAYS      TYPE I,
      AUSE      TYPE I,
      FERI      TYPE I,
      NBTRP     TYPE I,
      VALTT     LIKE PC207-BETRG,
      VALTT_TOT LIKE PC207-BETRG,
      TOTAL_TICKETS LIKE PC207-BETRG,
      REAL_DISC LIKE PC207-BETRG,
      sign(20)  type c.
DATA: END OF ALV_TAB.


DATA: BEGIN OF LAYOUT_HD_CITY OCCURS 0.
DATA: VERSAO      TYPE C LENGTH 4.
DATA: END OF LAYOUT_HD_CITY.


DATA: BEGIN OF LAYOUT_DT_CITY OCCURS 0.
DATA: CPF         TYPE C LENGTH 11,
      QUANT_DIAS  TYPE C LENGTH 2,
      VALOR_VT    TYPE C LENGTH 12,
      NOME_FUNC   LIKE PA0002-CNAME,
      DSGN        TYPE C LENGTH 1,
      APLIC       TYPE C LENGTH 3.
DATA: END OF LAYOUT_DT_CITY.


DATA: BEGIN OF LAYOUT_HD_BR OCCURS 0.
DATA: VERSAO      TYPE C LENGTH 4.
DATA: END OF LAYOUT_HD_BR.

DATA: BEGIN OF LAYOUT_DT_BR OCCURS 0.
DATA: CPF         TYPE C LENGTH 11,
      QUANT_DIAS  TYPE C LENGTH 2,
      VALOR_VT    TYPE C LENGTH 12,
      NOME_FUNC   LIKE PA0002-CNAME.
DATA: END OF LAYOUT_DT_BR.


DATA: BEGIN OF LAYOUT_HD_PIR OCCURS 0.
DATA: VERSAO      TYPE C LENGTH 4.
DATA: END OF LAYOUT_HD_PIR.

DATA: BEGIN OF LAYOUT_DT_PIR OCCURS 0.
DATA: CPF         TYPE C LENGTH 11,
      QUANT_DIAS  TYPE C LENGTH 2,
      VALOR_VT    TYPE C LENGTH 12,
      NOME_FUNC   LIKE PA0002-CNAME.
DATA: END OF LAYOUT_DT_PIR.
