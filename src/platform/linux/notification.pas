unit notification;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DBUS;

type

  RNotification = record
    id:       UInt32;
    Summary : string;
    Body    : string;
    IconName: string;
    TimeOut:  Integer;
    Urgency: Integer;
  end;

  { TNotificationClient }
  TNotificationClient = class
    private
      fAppName : string;
      FInitialized: boolean;
      fBus: PDBusConnection;
    public
      constructor Create;
      destructor Destroy; override;
      Function Init(AppName:string):boolean;
      Procedure UnInit;
      Function ShowNotification(var Notification: RNotification): UInt32;
      Property Initialized: boolean read FInitialized;
  end;

const
  (**
  * NOTIFY_EXPIRES_DEFAULT:  The default expiration time on a notification.
  * NOTIFY_EXPIRES_NEVER: The notification never expires. It stays open until closed by the calling API
                          or the user.
  *)
  NOTIFY_EXPIRES_DEFAULT = -1;
  NOTIFY_EXPIRES_NEVER   = 0;

  (**
  * NotifyUrgency:
  * @NOTIFY_URGENCY_LOW: Low urgency. Used for unimportant notifications.
  * @NOTIFY_URGENCY_NORMAL: Normal urgency. Used for most standard notifications.
  * @NOTIFY_URGENCY_CRITICAL: Critical urgency. Used for very important notifications.
  *
  * The urgency level of the notification.
  *)
  NOTIFY_URGENCY_LOW      = 0;
  NOTIFY_URGENCY_NORMAL   = 1;
  NOTIFY_URGENCY_CRITICAL = 2;


implementation
uses
  DbusExtension;
{ TNotificationClient }

const
  NOTIFY_DBUS_NAME           ='org.freedesktop.Notifications';
  NOTIFY_DBUS_CORE_INTERFACE ='org.freedesktop.Notifications';
  NOTIFY_DBUS_CORE_OBJECT    ='/org/freedesktop/Notifications';


constructor TNotificationClient.Create;
begin
  fBus := nil;
  FInitialized:= false;
end;

destructor TNotificationClient.Destroy;
begin
  inherited Destroy;
end;

function TNotificationClient.Init(AppName: string): boolean;
var
  msg: PDBusMessage;
  Err: DBusError;
begin
  fAppName := AppName;
  dbus_error_init(@Err);
  fBus := dbus_bus_get(DBUS_BUS_SESSION, @err);
  Result := CheckDbusError(Err, false);
  dbus_connection_set_exit_on_disconnect(fBus,dbus_bool_t(true));
  FInitialized:= Result;
end;

procedure TNotificationClient.UnInit;
begin
  dbus_connection_unref(fBus);
  fBus := nil;
  FInitialized:= false;
end;

function TNotificationClient.ShowNotification(var Notification: RNotification): UInt32;
var
  msg: PDBusMessage;
  reply : PDBusMessage;
  args: DBusMessageIter;
  s_val : pchar;
  i_val : Int32;
  U_val : UInt32;
  x_val : Int64;
  array_, dict, dict_entry, dict_val, variant_array: DBusMessageIter;
  property_: PChar;
  Err: DBusError;


begin
  msg:= dbus_message_new_method_call(NOTIFY_DBUS_NAME, NOTIFY_DBUS_CORE_OBJECT, NOTIFY_DBUS_CORE_INTERFACE,'Notify');
  dbus_message_iter_init_append(msg, @args);
  s_val:= pchar(fAppName);
  dbus_message_iter_append_basic(@args, DBUS_TYPE_STRING, @s_val );
  U_val:= Notification.id;
  dbus_message_iter_append_basic(@args, DBUS_TYPE_UINT32, @i_val );
  s_val:= pchar(Notification.IconName);
  dbus_message_iter_append_basic(@args, DBUS_TYPE_STRING, @s_val );
  s_val:= pchar(Notification.Summary);
  dbus_message_iter_append_basic(@args, DBUS_TYPE_STRING, @s_val );
  s_val:= pchar(Notification.Body);
  dbus_message_iter_append_basic(@args, DBUS_TYPE_STRING, @s_val );

  dbus_message_iter_open_container(@args, DBUS_TYPE_ARRAY, 's', @array_);
  dbus_message_iter_close_container(@args, @array_);


  dbus_message_iter_open_container(@args, DBUS_TYPE_ARRAY, '{sv}', @dict);

  dbus_message_iter_open_container(@dict, DBUS_TYPE_DICT_ENTRY, nil, @dict_entry);
  property_ := 'urgency';
  dbus_message_iter_append_basic(@dict_entry, DBUS_TYPE_STRING, @property_);
  dbus_message_iter_open_container(@dict_entry, DBUS_TYPE_VARIANT, 'x', @dict_val);
  x_val := Notification.Urgency;
  dbus_message_iter_append_basic(@dict_val, DBUS_TYPE_INT64, @x_val);
  dbus_message_iter_close_container(@dict_entry, @dict_val);
  dbus_message_iter_close_container(@dict, @dict_entry);
  dbus_message_iter_close_container(@args, @dict);

  i_val:= Notification.TimeOut;
  dbus_message_iter_append_basic(@args, DBUS_TYPE_INT32, @i_val );
  i_val := 1;
  dbus_error_init(@Err);
// Expected signature
//  (susssasa{sv}i)
  Reply := dbus_connection_send_with_reply_and_block(fBus, msg, -1, @Err);
  CheckDbusError(Err, true);

  if (dbus_message_iter_init(Reply, @args) > 0) then
    if (DBUS_TYPE_UINT32 = dbus_message_iter_get_arg_type(@args)) then
       begin
         dbus_message_iter_get_basic(@args, @i_val);
         Result:= i_val;
         Notification.id:= i_val;
       end;

  dbus_connection_flush(fBus);

  dbus_message_unref(msg);



end;

end.

