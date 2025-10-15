*&---------------------------------------------------------------------*
*&  Include           Z_1BNFE_MONITOR_I08
*&---------------------------------------------------------------------*
module process_input_0102 input.

* Get the texts for the selected cancellation reason

  read table gt_cancel_reason
       into  wa_cancel_reason
       with key reason = j_1bnfe_active-reason.

  move-corresponding wa_cancel_reason to j_1bnfe_cancelrt.


endmodule.                 " PROCESS_INPUT_0102  INPUT
