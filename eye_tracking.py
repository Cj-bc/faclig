#!/usr/bin/env python3
# code from https://qiita.com/mix_dvd/items/98feedc8c98bc7790b30


import cv2
import time
import shutil
import os
import atexit


def onexit():
    os.system('bash ./onexit.sh')


atexit.register(onexit)

cascade_file_root = "haarcascades"
face = {
        "part": "face",
        "cascade_file": "haarcascade_frontalface_default.xml"
       }
parts = [{
            "part": "right_eye",
            "cascade_file": "haarcascade_righteye_2splits.xml"},
         {
             "part": "left_eye",
             "cascade_file": "haarcascade_lefteye_2splits.xml"}
         ]



if __name__ == '__main__':
    ESC_KEY = 27
    INTERVAL = 33
    FRAME_RATE = 30
    TERM_SIZE = shutil.get_terminal_size()


    DEVICE_ID = 0

    # add cascade object to each dict in parts
    face.update({"cascade":
                 cv2.CascadeClassifier(f'{cascade_file_root}/{face["cascade_file"]}')})
    [part.update({"cascade":
                 cv2.CascadeClassifier(f'{cascade_file_root}/{part["cascade_file"]}')})
     for part in parts]

    cap = cv2.VideoCapture(DEVICE_ID)

    end_flag, c_frame = cap.read()
    height, width, channels = c_frame.shape


    while end_flag == True:

        img = c_frame
        img_gray = cv2.cvtColor(img, cv2.COLOR_BGR2GRAY)

        face_list = face['cascade'].detectMultiScale(img_gray, minSize=(100, 100))

        # if there's face, try to detect other parts on that area
        if len(face_list) != 0:
            # for each detected face, try. (MULTI FACE ISN'T SUPPORTED YET)
            for face_x, face_y, face_w, face_h in face_list:

                for part in parts:
                    part_pos = part['cascade'].detectMultiScale(img_gray, minSize=(100, 100))

                    # x: left > right
                    # y: bottom > top
                    for (x, y, w, h) in part_pos:

                        if face_x < x < (face_x + face_w) and \
                                face_y < y < (face_y + face_h):
                            term_pos_x = int(round(x / width * TERM_SIZE.columns))
                            term_pos_y = int(round(y / height * TERM_SIZE.lines))
                            os.system(f'bash ./draw.sh {term_pos_x} {term_pos_y} {part["part"]}')
                        else:
                            print('out of face!!!')
        time.sleep(0.5)

        key = cv2.waitKey(INTERVAL)
        if key == ESC_KEY:
            break

        end_flag, c_frame = cap.read()

    cv2.destroyAllWindows()
    cap.release()
