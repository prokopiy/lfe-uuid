(defmodule uuid
    ; (export (v4 0) (v3 2) (to_string 1) (to_binary 1) (version 1))
    (export all))


(defun v4 []
    (compose_uuid 4 2 (crypto:strong_rand_bytes 16)))


(defun v3
    (['dns Name] (when (is_list Name)) (create_namebased_uuid 'md5 (list_to_binary (list (binary (#x6ba7b8109dad11d180b400c04fd430c8 (size 128))) Name))))
    (['url Name] (when (is_list Name)) (create_namebased_uuid 'md5 (list_to_binary (list (binary (#x6ba7b8119dad11d180b400c04fd430c8 (size 128))) Name)))))


(defun v5
    (['dns Name] (when (is_list Name)) (create_namebased_uuid 'sha (list_to_binary (list (binary (#x6ba7b8109dad11d180b400c04fd430c8 (size 128))) Name))))
    (['url Name] (when (is_list Name)) (create_namebased_uuid 'sha (list_to_binary (list (binary (#x6ba7b8119dad11d180b400c04fd430c8 (size 128))) Name)))))



(defun compose_uuid
    "Compose UUID with input (hashed/random) data."
    ([Version Variant (binary (u0 (size 32)) (u1 (size 16)) (_ (size 4)) (u2 (size 12)) (_ (size 2)) (u3 (size 30)) (u4 (size 32)))]
        (binary (u0 (size 32)) (u1 (size 16)) (Version (size 4)) (u2 (size 12)) (Variant (size 2)) (u3 (size 30)) (u4 (size 32)))))

(defun create_namebased_uuid
    (['md5 Data] (when) (compose_uuid 3 2 (crypto:hash 'md5 Data)))
    (['sha Data] (when) (compose_uuid 5 2 (binary:part (crypto:hash 'sha Data) 0 16))))

(defun to_string [U]
    (lists:flatten 
        (io_lib:format "~8.16.0b-~4.16.0b-~4.16.0b-~2.16.0b~2.16.0b-~12.16.0b" (get_parts U))))

(defun get_parts
    ([(binary (TL (size 32)) (TM (size 16)) (THV (size 16)) (CSR (size 8)) (CSL (size 8)) (N (size 48)))] 
        (list TL TM THV CSR CSL N)))

(defun hex_to_int [Hex]
    (let (((tuple 'ok (list D) '()) (io_lib:fread "~16u" Hex)))
        D))

(defun to_binary
    ([UuidStr] (when (is_list UuidStr))
        (case (length UuidStr)
            (32 (when) (to_binary 'simple UuidStr))
            (36 (when) (to_binary 'pretty UuidStr))))
    ([_] (when) (erlang:error 'badarg)))

(defun to_binary
    (['simple UuidStr] (when) 
        (binary ((hex_to_int UuidStr) (size 128))))
    (['pretty UuidStr] (when) 
        (let ((P (lists:map #'hex_to_int/1 (string:tokens UuidStr "$-")))) 
            (binary 
                ((lists:nth 1 P) (size 32)) ((lists:nth 2 P) (size 16)) ((lists:nth 3 P) (size 16)) 
                ((lists:nth 4 P) (size 16)) ((lists:nth 5 P) (size 48))))))


(defun version
    ([(binary (_ (size 48)) (Version (size 4)) (_ (size 76)))] (when) Version)
    ([UuidStr] (when (is_list UuidStr)) (version (to_binary UuidStr)))
    ([_] (when) (erlang:error 'badarg)))

