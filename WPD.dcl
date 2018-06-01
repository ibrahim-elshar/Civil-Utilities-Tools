WPD : dialog {
      label = " WP v1.0 - Design and Draw Utilities Profiles";
	   
	   : spacer { width = 1;  height = 0.3;  } 
	
       : column {
        fixed_width = true;
	   //height = 50;
	   //width = 62;
         : column {
		 :row{
		 :column{
           : radio_column {
             label = "Input Mode";
			 key = "mode";
			   : radio_button { key = "ModeC"; label = "&Survey"; }
               : radio_button { key = "ModeA"; label = "&Automatic"; }
               : radio_button { key = "ModeM"; label = "&Manual"; }
			   : radio_button { key = "ModeS"; label = "Second &Run"; }
			   : radio_button { key = "ModeI"; label = "&Import"; }
                      
           }
		   : spacer { width = 2; }
		   : boxed_radio_column {
             label = "Slope Style";
			 key = "slopestyle";
               : radio_button { key = "sstyle1"; label = "&Decimal"; }
               : radio_button { key = "sstyle2"; label = "&Percent %"; }
			   : radio_button { key = "sstyle3"; label = "Per &thousand \U+2030"; }
			   			                        
            }
		   }

           : column {
		     : boxed_column {
             label = "Pipe Diameter";
		     : edit_box {
               key = "diameter";
               label = "Nominal Dia. (mm):";
			  // allow_accept = true;
               edit_width = 5;
               value = "";
             }
			 }
			 
           : boxed_column {
             label = "Profile Scale";
			   : edit_box {
               key = "vscale";
               label = "V.Scale 1:";
               edit_width = 5;
               value = "";
             }
             : edit_box {
               key = "hscale";
               label = "H.Scale 1:";
               edit_width = 5;
               value = "";
             }
			 }
			 : boxed_column {
             label = "Select Colors";
			
			//   : edit_box {
            //   key = "existingpipec";
            //   label = "Existing Pipe Color:";
            //   edit_width = 5;
            //   value = "";
            // }
			:row{
			//: popup_list {
            //    key = "epcolorlist";
            //    label = "Existing Pipe Color:";
			//	edit_width = 8;
				//fixed_width = true;
            //    value = "";
            // }
			  : text_part {				
                           label = "  Existing Pipe Color:";			
                       	 }
			   : image_button {
			         key = "epc";
                     width = 7;// X = 42
                     height = 1.2;// Y = 03
                     fixed_width = true;
                     fixed_height = true;
                     aspect_ratio = 1;
                     //color = 170;
                     //alignment = left; //centered
                   }
				   
			}
			
			 //  : edit_box {
             //  key = "temporarypipec";
             //  label = "Temporary Pipe Color:";
             //  edit_width = 5;
             //  value = "";
             // }
			 :row{
			  : text_part {				
                           label = "  Temporary Pipe Color:";			
                       	 }
			   : image_button {
			         key = "tpc";
                     width = 7;// X = 42
                     height = 1.2;// Y = 03
                     fixed_width = true;
                     fixed_height = true;
                     aspect_ratio = 1;
                     //color = 170;
                     //alignment = left; //centered
                   }
			}
			
			// : edit_box {
             //  key = "relocatedpipec";
             //  label = "Proposed Pipe Color:";
             //  edit_width = 5;
            //   value = "";
            // }
			 :row{
			  : text_part {				
                           label = "  Proposed Pipe Color:";			
                       	 }
			   : image_button {
			         key = "ppc";
                     width = 7;// X = 42
                     height = 1.2;// Y = 03
                     fixed_width = true;
                     fixed_height = true;
                     aspect_ratio = 1;
                     //color = 170;
                     //alignment = left; //centered
                   }
			} 
            } 

			  
			}
			}
			
			
			 : boxed_column {
             label = "Chainage";
			 :row{
			 :column{
			 : toggle { key = "allowchainage"; label = "Dra&w Chainage on Polyline"; }
			 : toggle { key = "allowchainageprofile"; label = "Add &Chainage to Profile"; }
			 		 
			 : edit_box {
               key = "startchainage";
               label = "Chainage Start Value:"; //label = "Chainage value at start of polyline:";
               edit_width = 5;
               value = "";
             }
			 : edit_box {
               key = "chainageinterval";
               label = "Chainage Marks Interval:"; //label = "Interval between chainage marks:";
               edit_width = 5;
               value = "";
             }
			: edit_box {
               key = "chtextsize";
               label = "Chainage text size:";
               edit_width = 5;
               value = "";
             }
			 }
			 
			 : boxed_radio_column{
             label = "Style";
			 key = "chainagestyle";
               : radio_button { key = "chstyle1"; label = "n.nnn"; }
               : radio_button { key = "chstyle2"; label = "n+nnn"; }
			   : radio_button { key = "chstyle3"; label = "n+nnn.nn"; }		                        
            }
				 
			 
			 }
			 }
			 
			 
		 : boxed_row {
             label = "Profile Annotation";
			 :boxed_column{
			 : toggle { key = "auto_anno_b"; label = "A&uto annotate elbows"; } 
			 
			   : edit_box {
                           key = "angtol";
                           label = "Allowable deflection angle:"; // label = "Allowable deflection angle:";
                           edit_width = 2;
                           value = "";
						   
			    
                    }
			 spacer_0;
			 spacer_0;
			 spacer_0;
			 
			 	 	}
						 
			: radio_column{
			   : radio_button { key = "copycros"; label ="Copy Anno."; } //"Copy Prof. Cros. && Anno."; 
               : radio_button { key = "selpanno"; label ="Select Anno."; }  //"Select Prof. Anno."; 
               : radio_button { key = "npanno"; label ="Neglect Anno."; }   //"Neglect Anno.";                
                        }	 
			  
			 }
			 					 
			 : boxed_row {
               : toggle { key = "exportdata"; label = "&Export data"; }
			   : button {
               key = "about";
               label = "A&bout";
               fixed_width = true; 
			   width = 15;
			  // height = 2;
			   alignment = centered;

             }
			   }
			
           : boxed_row {
		   //fixed_width = true;
           alignment = centered;
		   //: spacer { width = 1;  height = 0.0;  } 
             : button {
               key = "ok";
               label = "&OK";
               is_default = true;
			   fixed_width = true; 
			   width = 15;
			   //height = 2;
			   alignment = centered;
             }
			 //: spacer { width = 1;  height = 0.0;  } 
             : button {
               key = "cancel";
               label = "Cancel";
               is_default = false;
               is_cancel = true;
			   fixed_width = true;
			   width = 15;
			  // height = 2;
			   alignment = centered;
	          }
			 //: spacer { width = 1;  height = 0.0;  } 
			 //help_button;
			 : button {
               key = "help";
               label = "&Help";
               is_default = false;
               fixed_width = true;
			   width = 15;
			  // height = 2;
			   alignment = centered;
	          }
			//: spacer { width = 0;  height = 0.5;  }
           }
//		   :row{
//           : image {
//                     key = "WPlogo";
//                     width = 2;// X = 42
//                     height = 1.5;// Y = 03
//                     fixed_width = true;
//                     fixed_height = true;
//                     aspect_ratio = 1;
//                     color = 170;
//                     alignment = left; //centered
//                   }
				   
//	: paragraph {				
//     alignment = left;
//	 : spacer { width = 0;  height = 0.05;  } 
//                 : text_part {				
//                             label = "Designed and Created by Ibrahim El Sharr";		
//	                         }						
 
//               // : text_part {				
//               //            label = "by Ibrahim El Sharr";			
//               //        	 }						
//    
//                }
//				: spacer { width = 5;  height = 0.0;  } 
//             }	 
         }
       }    
}
