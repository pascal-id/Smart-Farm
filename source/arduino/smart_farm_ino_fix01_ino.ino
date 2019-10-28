#include <WiFi.h>
#include <WiFiClient.h>
#include <WiFiServer.h>
#include <WiFiUdp.h>
#include <SPI.h>
#include <OneWire.h>
#include <DallasTemperature.h>
#include <Time.h>

char ssid[] = "LeleHotspot"; // your network SSID (name) 
char pass[] = "leletampan"; // your network password
int keyIndex = 0;                 // your network key Index number (needed only for WEP)
String myStr;
boolean reading = false;
String mod;
int kran= 2;
int thn;
int bln;
int tgl;
int jam;
int men;
int det;
int sensorPin = A0;  // pin sensor
int powerPin = 8; 
int pinLM35 = A1;
int counter = 0;
float temperature = 00;
int temperature1=00;
String status_kran;
int status = WL_IDLE_STATUS;

WiFiServer server(80);

#define ONE_WIRE_BUS 3

// Setting oneWire untuk siap berkomunikasi
OneWire oneWire(ONE_WIRE_BUS);

// melewatkan referensi oneWire untuk terhubung dengan DallasTemeperature
DallasTemperature sensors(&oneWire);

void setup() {  
  Serial.begin(9600);
  pinMode(kran, OUTPUT);
  digitalWrite(kran, HIGH);

  // check for the presence of the shield:
  if (WiFi.status() == WL_NO_SHIELD) {
    Serial.println("No WiFi_"); 
    // don't continue:
    while(true);
  } 
  
  // attempt to connect to Wifi network:
  while ( status != WL_CONNECTED) { 
    Serial.print("_Connecting to ");
    Serial.print(ssid);
    Serial.println("_");
    
    // Connect to WPA/WPA2 network. Change this line if using open or WEP network:    
    status = WiFi.begin(ssid, pass);

    // wait 10 seconds for connection:
    delay(10000);
  } 
  
  Serial.print("_Connected with IP : ");
  Serial.print(WiFi.localIP());
  Serial.println("_");
  server.begin();
  sensors.begin();
  // you're connected now, so print out the status:
}


void loop() {
  // listen for incoming clients
  WiFiClient client = server.available();
  
  if (client) {
    // an http request ends with a blank line
    boolean currentLineIsBlank = true;
    boolean sentHeader = false;
    myStr = "";
    while (client.connected()) {
      if (client.available()) {

        char c = client.read();

        if(reading && c == ' ') reading = false;
        if(c == '?') reading = true; //found the ?, begin reading the info

        if(reading){
          //Serial.print(c);
          if (c!='?') {
            myStr += c;
          }

        }

        if (c == '\n' && currentLineIsBlank)  break;

        if (c == '\n') {
          currentLineIsBlank = true;
        }else if (c != '\r') {
          currentLineIsBlank = false;
        }
      }
    }
    
    mod = getParams("&" + myStr + "&", "module");
    
    client.println("HTTP/1.1 200 OK");
    client.println("Content-Type: text/html");
    client.println();

    if(mod == "kelembaban") {
      // hidupkan power
      digitalWrite(powerPin, HIGH);
      delay(500);
      //baca nilai analog dari sensor
      int nilaiSensor = analogRead(sensorPin);
  
      digitalWrite(powerPin, LOW);
      //makin lembab maka makin tinggi nilai outputnya
      //nilaiSensor = 1023 - nilaiSensor;
     
      Serial.print("Info Kelembaban");
      client.print("{\"status\":\"success\",\"message\":\"Info Kelembaban\",\"data\":{\"nilai\":\"");
      client.print(nilaiSensor);
      client.print("\"}");
    } else
        
    if(mod == "suhu") {
      sensors.requestTemperatures(); 
      //rumus yang seperti program pertama
      temperature = sensors.getTempCByIndex(0);
      
      Serial.print("Info Suhu");
      client.print("{\"status\":\"success\",\"message\":\"Info Suhu\",\"data\":{\"nilai\":\"");
      client.print(temperature);
      client.print("\"}");
    } else
    
    if(mod == "status-kran") {      
      if(digitalRead(kran) == LOW)
      status_kran = "nyala"; else
      status_kran = "mati";
      
      Serial.print("Status Kran");
      client.print("{\"status\":\"success\",\"message\":\"Status Kran\",\"data\":{\"status\":\"");
      client.print(status_kran);
      client.print("\"}");
    } else

    if(mod == "get-time") {      
      Serial.print("Get Time");
      client.print("{\"status\":\"success\",\"message\":\"Get Time\",\"data\":{\"nilai\":\"");
      client.print(hour());
      client.print(":");
      client.print(minute());
      client.print(":");
      client.print(second());
      client.print("\"}");
    } else
   
    if(mod == "buka-kran") {
      Serial.print("Buka Kran");
      digitalWrite(kran, LOW);
      client.print("{\"status\":\"success\",\"message\":\"Buka Kran\",\"data\":{}");  
    } else 
    
    if(mod == "tutup-kran") {
      Serial.print("Tutup Kran");
      digitalWrite(kran, HIGH);
      client.print("{\"status\":\"success\",\"message\":\"Tutup Kran\",\"data\":{}");
    } else 
    
    if(mod == "set-time") {
      thn = getParams("&" + myStr + "&", "thn").toInt();
      bln = getParams("&" + myStr + "&", "bln").toInt();
      tgl = getParams("&" + myStr + "&", "tgl").toInt(); 
      jam = getParams("&" + myStr + "&", "jam").toInt();
      men = getParams("&" + myStr + "&", "men").toInt();
      det = getParams("&" + myStr + "&", "det").toInt();
      setTime(jam, men, det, tgl, bln, thn);
      
      Serial.print("Set Time");
      client.print("{\"status\":\"success\",\"message\":\"Set Time\",\"data\":{");
      client.print("\"jam\":\"");
      client.print(jam);
      client.print("\",");
      client.print("\"menit\":\"");
      client.print(men);
      client.print("\",");
      client.print("\"detik\":\"");
      client.print(det);
      client.print("\",");
      client.print("\"tanggal\":\"");
      client.print(tgl);
      client.print("\",");
      client.print("\"bulan\":\"");
      client.print(bln);
      client.print("\",");
      client.print("\"tahun\":\"");
      client.print(thn);
      client.print("\"");
      client.print("}");
    } else    
    
    {
      Serial.print("No Specified Module");
      client.print("{\"status\":\"error\",\"message\":\"module ");
      client.print(mod);
      client.print(" not found\",\"data\":{\"\"}");
    }
    
    client.print(",\"param\":\"");
    client.print(myStr);
    client.println("\"}");
    Serial.print(" ");
    Serial.print(myStr);
    Serial.println("_");
    myStr = "";
    
    delay(1000); // give the web browser time to receive the data
    client.stop(); // close the connection:    
  } 
    
  delay(100);
  counter++;
}

String getParams(String AText, String AIndex) {
  int xxx;
  String sss;

  xxx = AText.indexOf("&" + AIndex + "=");
  
  if(xxx >= 0) {      

    sss = AText.substring(xxx + 1, AText.length());
    
    xxx = sss.indexOf("&");

    if(xxx >=0)
    sss = sss.substring(0, xxx);
    else return "";
    
    xxx = sss.indexOf("=");
    
    if(xxx >=0)
    sss = sss.substring(xxx + 1, sss.length());
    else return "";
    
    return sss;
  } return "";
}

