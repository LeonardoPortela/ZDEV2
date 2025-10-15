* she020206 initialize gt_result before calculate the profit and loss
*           result
* she023816 refresh of gt_result and gt_change_pos_data
*           is necessary, if the function is called several times (for
*           example from active excel)
FUNCTION ZBSPL_DATA_PREPARE .
*"----------------------------------------------------------------------
*"*"Interface global:
*"  IMPORTING
*"     VALUE(IS_SETTINGS) LIKE  RFBILA_ALV_SETTINGS STRUCTURE
*"        RFBILA_ALV_SETTINGS
*"  TABLES
*"      IT_RFBILA_ALV_DATA STRUCTURE  ZRFBILA_ALV_DATA
*"      IT_RANGE_ACCOUNTS OPTIONAL
*"----------------------------------------------------------------------

*                                                        begin she023816
  refresh gt_result.
  refresh gt_change_pos_data.
*                                                          end she023816
   SELECT SINGLE * FROM T011
                  WHERE VERSN = IS_SETTINGS-FS_VERSION.
   SELECT SINGLE * FROM T011T
                  WHERE SPRAS = IS_SETTINGS-FS_LANGUAGE
                    AND VERSN = IS_SETTINGS-FS_VERSION.

*. find the accounts which are
*. not assigned to a bs/p&l item
   PERFORM BSPL_NOT_ASSIGNED_ACC_FIND
                          TABLES IT_RFBILA_ALV_DATA
                          USING  IS_SETTINGS.
*. prepare selected data with BS/P&L-items
   PERFORM BSPL_ITEMS_ADD
                          TABLES IT_RFBILA_ALV_DATA
                          USING  IS_SETTINGS-ALTACCT.

   APPEND LINES OF GT_CHANGE_POS_DATA
                TO IT_RFBILA_ALV_DATA.

*. prepare the change positions of reporting period
   PERFORM BSPL_CHANGE_POSITIONS_PREPARE
                          TABLES IT_RFBILA_ALV_DATA
                          USING  '1'.
*. prepare the change positions of comparison period
   PERFORM BSPL_CHANGE_POSITIONS_PREPARE
                          TABLES IT_RFBILA_ALV_DATA
                          USING  '2'.

   IF IS_SETTINGS-TREE       = CON_X
  AND IS_SETTINGS-STRUCBLNCE = CON_X.
*    structured balance list, do not calculate
*    profit and loss
   ELSE.
*... calculate the profit and loss (result)
     PERFORM BSPL_RESULT_CALCULATE
                            TABLES IT_RFBILA_ALV_DATA.
*... prepare the bs result position of reporting period
     PERFORM BSPL_CHANGE_POSITIONS_PREPARE
                            TABLES GT_RESULT
                            USING  '1'.
*... prepare the bs result position of comparison period
     PERFORM BSPL_CHANGE_POSITIONS_PREPARE
                            TABLES GT_RESULT
                            USING  '2'.
     APPEND LINES OF GT_RESULT
                  TO IT_RFBILA_ALV_DATA.
   ENDIF.
*.
   PERFORM BSPL_DATA_PREPARE
                          TABLES IT_RFBILA_ALV_DATA
                                 GT_BSPLDATA
                          USING  IS_SETTINGS.

  IF IS_SETTINGS-REPID = CON_RFBILA10.
    IF NOT ( IS_SETTINGS-ZEROACCT IS INITIAL ).
*.... display accounts with zero balance too
      PERFORM BSPL_ZERO_BLNCE_ACCOUNTS_ADD
                             TABLES GT_BSPLDATA
                                    GT_RSTHIE
                                    IT_RANGE_ACCOUNTS
                             USING  IS_SETTINGS-ALTACCT.
* begin "n1592941
* Move logic regarding zero balance accounts to RFBILA10
*    ELSE.
**.... delete accounts whose totals records balance to zero
*      PERFORM BSPL_ACCT_ZERO_BLNCE_DELETE
*                                TABLES GT_BSPLDATA.
* end "n1592941
    ENDIF.
  ENDIF.

* thin out nodes without children
  PERFORM BSPL_NODES_THIN_OUT
                         TABLES GT_BSPLDATA
                                GT_RSTHIE
                         USING  IS_SETTINGS-ALTACCT.

ENDFUNCTION.
