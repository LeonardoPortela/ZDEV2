FUNCTION-POOL zabap_util.                   "MESSAGE-ID ..

DATA dref TYPE REF TO data.

FIELD-SYMBOLS: <fs_t>   TYPE STANDARD TABLE,
               <fs_any> TYPE ANY TABLE.

DATA: gt_jdoc TYPE TABLE OF zsds0040,
      wa_jdoc TYPE zsds0040.

* INCLUDE LZABAP_UTILD...                    " Local class definition
