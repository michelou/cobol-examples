<!-- FlyByNight Travel Agency Sorted Summary File (Exam Question) -->

<TABLE width="694" border="1" cellspacing="0" cellpadding="5">
  <TR> 
    <TD bgcolor="#990000" valign="middle" height="27" colspan="3"> 
      <H3 align="center"><FONT color="#FFFF00">Fly By Night Travel Agency</FONT><FONT color="#FFFF00"><BR>
        Sorted Summary File</FONT></H3>
    </TD>
  </TR>
  <TR> 
    <TD bgcolor="#FFFFFF" valign="middle" height="22" colspan="2"><B>Time to complete</B></TD>
    <TD bgcolor="#FFFFFF" width="78%" valign="middle" height="22">Allow 4-5 hours 
      continuous</TD>
  </TR>
  <TR> 
    <TD bgcolor="#FFFFFF" valign="middle" height="8" colspan="2"><B>Program Download</B></TD>
    <TD bgcolor="#FFFFFF" width="78%" valign="middle" height="8"><A href="FlyByNight.CBL">FlyByNight.cbl</A> 
      is a model answer. Don't look at this until you have made your own attempt 
      at the program.</TD>
  </TR>
  <TR> 
    <TD bgcolor="#FFFFFF" valign="middle" height="8" colspan="2"><B>Example Output 
      </B></TD>
    <TD bgcolor="#FFFFFF" width="78%" valign="middle" height="8"> 
      <P><A href="SUMMARY.DAT">Summary.dat</A> (The sorted Summary file)<BR>
        <A href="BOOKSORT.DAT">BookSort.dat</A> (The sorted Bookings file with 
        non-tourists removed)</P>
      </TD>
  </TR>
  <TR> 
    <TD bgcolor="#FFFFFF" valign="middle" height="22" colspan="2"><B>Example Input</B></TD>
    <TD bgcolor="#FFFFFF" width="78%" valign="middle" height="22"> 
      <P><A href="STUDPAY.DAT">Bookings.dat</A> (The unsorted Bookings file) </P>
    </TD>
  </TR>
  <TR> 
    <TD bgcolor="#FFFFFF" valign="middle" height="14" colspan="2"><B>Major Constructs</B></TD>
    <TD bgcolor="#FFFFFF" width="78%" valign="middle" height="14">Indexed Files, 
      Print Files</TD>
  </TR>
</table>

## <span id="introduction">Introduction</span>

The FlyByNight travel agency sends its customers all over the world. 
For each booking a record is created in the Booking Master file (`BOOKING.DAT`). 
Each customer booking is assigned (as part of the record) a category indicating the primary reason for the booking (i.e. Sport, Tourism, Business, Personal and Other). The file is unsorted and each record has the following description;

<TABLE border=1 width="400" style="margin-left:10%;">
  <TR bgcolor="#FFFFCC"> 
    <TD style="text-align:center;font-weight:bold;">Field</TD>
    <TD style="text-align:center;font-weight:bold;">Type</TD>
    <TD style="text-align:center;font-weight:bold;">Length</TD>
    <TD style="text-align:center;font-weight:bold;">Value</TD>
  </TR>
  <TR>
    <TD>Customer-Name</TD>
    <TD style="text-align:center;">X</TD>
    <TD style="text-align:center;">30</TD>
    <TD style="text-align:center;">upper &amp; lower case</TD>
  </TR>
  <TR>
    <TD>Destination-Name</TD>
    <TD style="text-align:center;">X</TD>
    <TD style="text-align:center;">20</TD>
    <TD style="text-align:center;">upper &amp; lower case</TD>
  </TR>
  <TR>
    <TD>Booking-Charge</TD>
    <TD style="text-align:center;">N</TD>
    <TD style="text-align:center;">7</TD>
    <TD style="text-align:center;">10.00 - 99999.99</TD>
  </TR>
  <TR>
    <TD>Num-Of-Males</TD>
    <TD style="text-align:center;">N</TD>
    <TD style="text-align:center;">2</TD>
    <TD style="text-align:center;">0 - 99</TD>
  </TR>
  <TR>
    <TD>Num-Of-Females</TD>
    <TD style="text-align:center;">N</TD>
    <TD style="text-align:center;">2</TD>
    <TD style="text-align:center;">0 - 99</TD>
  </TR>
  <TR>
    <TD>Num-Of-Children</TD>
    <TD style="text-align:center;">N</TD>
    <TD style="text-align:center;">2</TD>
    <TD style="text-align:center;">0 - 99</TD>
  </TR>
  <TR>
    <TD>Category</TD>
    <TD style="text-align:center;">X</TD>
    <TD style="text-align:center;">1</TD>
    <TD style="text-align:center;">S/T/B/P/O</TD>
  </TR>
</TABLE>

## The Task

A program is required which will read the Booking Master file to produce a Summary file sequenced upon ascending Destination-Name. Each record in this file will be a summary of all the Tourist records in the Booking file which list a particular location as their destination. The record will show the Destination-Name, the Total-Receipts from tourists travelling to that destination and the Total-Males, Total-Females and Total-Children who make up that tourist population. Each record in the Summary file has the description shown below;

<TABLE border=1 width="400" style="margin:0 10%; 0 10%;">
  <TR bgcolor="#FFFFCC">
    <TD style="text-align:center;font-weight:bold;">Field</TD>
    <TD style="text-align:center;font-weight:bold;">Type</TD>
    <TD style="text-align:center;font-weight:bold;">Length</TD>
    <TD style="text-align:center;font-weight:bold;">Value</TD>
  </TR>
  <TR>
    <TD>Destination-Name</TD>
    <TD style="text-align:center;">X</TD>
    <TD style="text-align:center;">20</TD>
    <TD style="text-align:center;">upper case only</TD>
  </TR>
  <TR>
    <TD>Total-Receipts</TD>
    <TD style="text-align:center;">N</TD>
    <TD style="text-align:center;">10</TD>
    <TD style="text-align:center;">10.00 - 99999999.99</TD>
  </TR>
  <TR>
    <TD>Total-Males</TD>
    <TD style="text-align:center;">N</TD>
    <TD style="text-align:center;">6</TD>
    <TD style="text-align:center;">0 - 999999</TD>
  </TR>
  <TR>
    <TD>Total-Females</TD>
    <TD style="text-align:center;">N</TD>
    <TD style="text-align:center;">6</TD>
    <TD style="text-align:center;">0 - 999999</DIV></TD>
  </TR>
  <TR>
    <TD>Total-Children</TD>
    <TD style="text-align:center;">N</TD>
    <TD style="text-align:center;">6</TD>
    <TD style="text-align:center;">0 - 999999</TD>
  </TR>
</TABLE>

### The Danger Levy

As well as the Booking-Charge, tourists travelling to some destinations will incur an additional insurance charge. This will be levied as a percentage of the Booking-Charge and will be added to it to give a Total-Charge for the booking (e.g. Charge = $250.00, Percentage Insurance = 15%, Total-Charge = $287.50). Listed below are affected countries as well as the insurance charges that travelling to them will incur.

<p style="text-align:center;"">
Afghanistan 50%, Cambodia 24%, Corsica 18%, El Salvador 85%,<br/>
Haiti 21%, Honduras 23%, Israel 11%, Iran 57%, Iraq 33%,
</p>

Note that the Destination-Name in the Booking Master File may be all upper case or all lower case or a mixture. Note also that &quot;France&quot; is not equal to &quot;FRANCE&quot;.

### Suggested Procedure

Sort the Booking Master file into Destination-Name order, eliminating all non-tourist records and changing the Destination-Name to upper case before the actual sort is done.

For each group of records for a particular Destination;

<blockquote>
Sum all the Males, Females and Children.<br/>
Sum all the Booking-Charges.<br/>
When all the records for the group have been processed check to see if the destination is in the insurance surcharge group. If it is, work out the insurance charge and add it to the Total-Booking-Charges giving the Total-Receipts. <br/>
Set up the output record and write it to the Summary File.
</blockquote>

<p style="margin:50px 0 0 0;"><hr/></p>
<P style="text-align:center;font-size:-1;font-weight:bold;">Copyright Notice</P>
<P style="font-size:-1;">
This COBOL project specification is the copyright property of Michael Coughlan. You have permission to use this material for your own personal use but you may not reproduce it in any published work without written permission from the author.</FONT>
</P>
